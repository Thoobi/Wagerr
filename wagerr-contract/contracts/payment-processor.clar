;; Payout Processor Smart Contract
;; Automated payout processing and settlement system

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-BET (err u101))
(define-constant ERR-INVALID-EVENT (err u102))
(define-constant ERR-PAYOUT-ALREADY-PROCESSED (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-PAYOUT-FAILED (err u105))
(define-constant ERR-EVENT-NOT-SETTLED (err u106))
(define-constant ERR-INVALID-WINNER-LIST (err u107))

;; Data Variables
(define-data-var contract-balance uint u0)
(define-data-var total-payouts-processed uint u0)
(define-data-var payout-fee-percentage uint u250) ;; 2.5% fee

;; Data Maps
(define-map bets
  { bet-id: uint }
  {
    bettor: principal,
    event-id: uint,
    amount: uint,
    odds: uint,
    prediction: (string-ascii 50),
    status: (string-ascii 20), ;; "pending", "won", "lost", "paid"
    created-at: uint
  }
)

(define-map events
  { event-id: uint }
  {
    name: (string-ascii 100),
    outcome: (optional (string-ascii 50)),
    status: (string-ascii 20), ;; "active", "settled", "cancelled"
    total-pool: uint,
    settled-at: (optional uint)
  }
)

(define-map payouts
  { payout-id: uint }
  {
    bet-id: uint,
    winner: principal,
    amount: uint,
    fee: uint,
    status: (string-ascii 20), ;; "pending", "completed", "failed"
    processed-at: (optional uint),
    retry-count: uint
  }
)

(define-map payout-history
  { winner: principal, event-id: uint }
  {
    total-winnings: uint,
    payout-count: uint,
    last-payout: uint
  }
)

;; Sequence counters
(define-data-var next-bet-id uint u1)
(define-data-var next-event-id uint u1)
(define-data-var next-payout-id uint u1)

;; Public Functions

;; Process payouts for a specific event
(define-public (process-payouts (event-id uint))
  (let (
    (event-data (unwrap! (map-get? events { event-id: event-id }) ERR-INVALID-EVENT))
  )
    (asserts! (is-eq (get status event-data) "settled") ERR-EVENT-NOT-SETTLED)
    (asserts! (is-some (get outcome event-data)) ERR-EVENT-NOT-SETTLED)
    
    ;; Process all winning bets for this event
    (ok (settle-winning-bets event-id (unwrap-panic (get outcome event-data))))
  )
)

;; Calculate winnings for a specific bet
(define-public (calculate-winnings (bet-id uint))
  (let (
    (bet-data (unwrap! (map-get? bets { bet-id: bet-id }) ERR-INVALID-BET))
    (bet-amount (get amount bet-data))
    (odds (get odds bet-data))
  )
    (ok {
      bet-id: bet-id,
      stake: bet-amount,
      gross-winnings: (* bet-amount odds),
      fee: (/ (* (* bet-amount odds) (var-get payout-fee-percentage)) u10000),
      net-winnings: (- (* bet-amount odds) (/ (* (* bet-amount odds) (var-get payout-fee-percentage)) u10000))
    })
  )
)

;; Distribute winnings to a list of winners
(define-public (distribute-winnings (winner-list (list 1000 { winner: principal, bet-id: uint, amount: uint })))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> (len winner-list) u0) ERR-INVALID-WINNER-LIST)
    
    (ok (fold process-single-payout winner-list u0))
  )
)

;; Handle failed payout retry
(define-public (handle-failed-payout (bet-id uint))
  (let (
    (bet-data (unwrap! (map-get? bets { bet-id: bet-id }) ERR-INVALID-BET))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status bet-data) "won") ERR-PAYOUT-ALREADY-PROCESSED)
    
    ;; Retry payout processing
    (match (calculate-winnings bet-id)
      success (begin
        (unwrap! (execute-payout 
          (get bet-id success)
          (get bettor bet-data)
          (get net-winnings success)
          (get fee success)
        ) ERR-PAYOUT-FAILED)
        (ok true)
      )
      error (err error)
    )
  )
)

;; Get payout status for a bet
(define-read-only (get-payout-status (bet-id uint))
  (let (
    (bet-data (map-get? bets { bet-id: bet-id }))
  )
    (match bet-data
      bet-info (ok {
        bet-id: bet-id,
        status: (get status bet-info),
        bettor: (get bettor bet-info),
        amount: (get amount bet-info)
      })
      (err ERR-INVALID-BET)
    )
  )
)

;; Get payout history for a user
(define-read-only (get-user-payout-history (user principal) (event-id uint))
  (map-get? payout-history { winner: user, event-id: event-id })
)

;; Administrative Functions

;; Create a new bet (for testing/admin purposes)
(define-public (create-bet (bettor principal) (event-id uint) (amount uint) (odds uint) (prediction (string-ascii 50)))
  (let (
    (bet-id (var-get next-bet-id))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    (map-set bets
      { bet-id: bet-id }
      {
        bettor: bettor,
        event-id: event-id,
        amount: amount,
        odds: odds,
        prediction: prediction,
        status: "pending",
        created-at: stacks-block-height
      }
    )
    
    (var-set next-bet-id (+ bet-id u1))
    (ok bet-id)
  )
)

;; Create a new event
(define-public (create-event (name (string-ascii 100)))
  (let (
    (event-id (var-get next-event-id))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    (map-set events
      { event-id: event-id }
      {
        name: name,
        outcome: none,
        status: "active",
        total-pool: u0,
        settled-at: none
      }
    )
    
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

;; Settle an event with outcome
(define-public (settle-event (event-id uint) (outcome (string-ascii 50)))
  (let (
    (event-data (unwrap! (map-get? events { event-id: event-id }) ERR-INVALID-EVENT))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status event-data) "active") ERR-INVALID-EVENT)
    
    (map-set events
      { event-id: event-id }
      (merge event-data {
        outcome: (some outcome),
        status: "settled",
        settled-at: (some stacks-block-height)
      })
    )
    
    (ok true)
  )
)

;; Private Functions

;; Settle winning bets for an event
(define-private (settle-winning-bets (event-id uint) (winning-outcome (string-ascii 50)))
  (begin
    ;; This would iterate through all bets for the event
    ;; For demonstration, we'll return success
    ;; In a real implementation, you'd need to query all bets for this event
    (var-set total-payouts-processed (+ (var-get total-payouts-processed) u1))
    true
  )
)

;; Process losing bets
(define-private (process-losing-bets (event-id uint) (losing-outcome (string-ascii 50)))
  (begin
    ;; Mark losing bets as "lost"
    ;; Update contract balance with losing bet amounts
    true
  )
)

;; Update bet status
(define-private (update-bet-status (bet-id uint) (new-status (string-ascii 20)))
  (match (map-get? bets { bet-id: bet-id })
    bet-data (begin
      (map-set bets
        { bet-id: bet-id }
        (merge bet-data { status: new-status })
      )
      true
    )
    false
  )
)

;; Process a single payout
(define-private (process-single-payout (payout-info { winner: principal, bet-id: uint, amount: uint }) (acc uint))
  (let (
    (payout-id (var-get next-payout-id))
    (fee (/ (* (get amount payout-info) (var-get payout-fee-percentage)) u10000))
    (net-amount (- (get amount payout-info) fee))
  )
    (map-set payouts
      { payout-id: payout-id }
      {
        bet-id: (get bet-id payout-info),
        winner: (get winner payout-info),
        amount: net-amount,
        fee: fee,
        status: "pending",
        processed-at: none,
        retry-count: u0
      }
    )
    
    (var-set next-payout-id (+ payout-id u1))
    
    ;; Execute the actual payout with proper response handling
    (if (is-ok (execute-payout (get bet-id payout-info) (get winner payout-info) net-amount fee))
      (+ acc u1)
      acc
    )
  )
)

;; Execute payout to winner
(define-private (execute-payout (bet-id uint) (winner principal) (amount uint) (fee uint))
  (begin
    ;; Check if bet exists and is in correct state
    (match (map-get? bets { bet-id: bet-id })
      bet-data (begin
        ;; In a real implementation, this would transfer STX to the winner
        ;; For now, we'll simulate the payout
        (update-bet-status bet-id "paid")
        
        ;; Update payout history
        (match (map-get? payout-history { winner: winner, event-id: (get event-id bet-data) })
          existing-history (map-set payout-history
            { winner: winner, event-id: (get event-id bet-data) }
            {
              total-winnings: (+ (get total-winnings existing-history) amount),
              payout-count: (+ (get payout-count existing-history) u1),
              last-payout: stacks-block-height
            }
          )
          (map-set payout-history
            { winner: winner, event-id: (get event-id bet-data) }
            {
              total-winnings: amount,
              payout-count: u1,
              last-payout: stacks-block-height
            }
          )
        )
        
        (ok true)
      )
      (err ERR-INVALID-BET)
    )
  )
)

;; Read-only functions for contract state
(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

(define-read-only (get-total-payouts-processed)
  (var-get total-payouts-processed)
)

(define-read-only (get-payout-fee-percentage)
  (var-get payout-fee-percentage)
)

(define-read-only (get-bet-details (bet-id uint))
  (map-get? bets { bet-id: bet-id })
)

(define-read-only (get-event-details (event-id uint))
  (map-get? events { event-id: event-id })
)
