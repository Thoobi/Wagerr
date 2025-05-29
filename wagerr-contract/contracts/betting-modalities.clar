;; Betting Modalities Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-bet-not-found (err u101))
(define-constant err-bet-already-exists (err u102))
(define-constant err-bet-closed (err u103))
(define-constant err-bet-not-closed (err u104))
(define-constant err-bet-already-settled (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-invalid-outcome (err u107))
(define-constant err-already-bet (err u108))
(define-constant err-invalid-amount (err u109))
(define-constant err-not-participant (err u110))

;; Define data maps
(define-map bets
  { id: uint }
  {
    creator: principal,
    description: (string-ascii 256),
    possible-outcomes: (list 10 (string-ascii 64)),
    total-amount: uint,
    min-bet-amount: uint,
    max-participants: uint,
    current-participants: uint,
    is-peer-to-peer: bool,
    is-closed: bool,
    is-settled: bool,
    winning-outcome: (optional (string-ascii 64)),
    created-at: uint
  }
)

(define-map bet-participants
  { bet-id: uint, participant: principal }
  {
    amount: uint,
    outcome: (string-ascii 64)
  }
)

(define-map outcome-totals
  { bet-id: uint, outcome: (string-ascii 64) }
  {
    total-amount: uint,
    participants: uint
  }
)

;; Define variables
(define-data-var bet-counter uint u0)

;; Create a new peer-to-peer bet
(define-public (create-peer-to-peer-bet (description (string-ascii 256)) (possible-outcomes (list 10 (string-ascii 64))) (bet-amount uint))
  (let
    (
      (bet-id (+ (var-get bet-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (> bet-amount u0) (err err-invalid-amount))
    (asserts! (> (len possible-outcomes) u1) (err err-invalid-outcome))
    
    (map-set bets
      { id: bet-id }
      {
        creator: tx-sender,
        description: description,
        possible-outcomes: possible-outcomes,
        total-amount: u0,
        min-bet-amount: bet-amount,
        max-participants: u2,
        current-participants: u0,
        is-peer-to-peer: true,
        is-closed: false,
        is-settled: false,
        winning-outcome: none,
        created-at: current-block
      }
    )
    
    (var-set bet-counter bet-id)
    (ok bet-id)
  )
)

;; Create a new multi-user bet
(define-public (create-multi-user-bet (description (string-ascii 256)) (possible-outcomes (list 10 (string-ascii 64))) (min-bet-amount uint) (max-participants uint))
  (let
    (
      (bet-id (+ (var-get bet-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (> min-bet-amount u0) (err err-invalid-amount))
    (asserts! (> max-participants u2) (err err-invalid-amount))
    (asserts! (> (len possible-outcomes) u1) (err err-invalid-outcome))
    
    (map-set bets
      { id: bet-id }
      {
        creator: tx-sender,
        description: description,
        possible-outcomes: possible-outcomes,
        total-amount: u0,
        min-bet-amount: min-bet-amount,
        max-participants: max-participants,
        current-participants: u0,
        is-peer-to-peer: false,
        is-closed: false,
        is-settled: false,
        winning-outcome: none,
        created-at: current-block
      }
    )
    
    (var-set bet-counter bet-id)
    (ok bet-id)
  )
)

;; Place a bet
(define-public (place-bet (bet-id uint) (outcome (string-ascii 64)) (amount uint))
  (let
    (
      (bet (unwrap! (map-get? bets { id: bet-id }) (err err-bet-not-found)))
      (existing-bet (map-get? bet-participants { bet-id: bet-id, participant: tx-sender }))
    )
    ;; Check if bet is still open
    (asserts! (not (get is-closed bet)) (err err-bet-closed))
    ;; Check if user hasn't already placed a bet
    (asserts! (is-none existing-bet) (err err-already-bet))
    ;; Check if bet amount is valid
    (asserts! (>= amount (get min-bet-amount bet)) (err err-invalid-amount))
    ;; Check if outcome is valid
    (asserts! (is-some (index-of (get possible-outcomes bet) outcome)) (err err-invalid-outcome))
    ;; Check if there's room for more participants
    (asserts! (< (get current-participants bet) (get max-participants bet)) (err err-bet-closed))
    
    ;; Update bet participants
    (map-set bet-participants
      { bet-id: bet-id, participant: tx-sender }
      {
        amount: amount,
        outcome: outcome
      }
    )
    
    ;; Update outcome totals
    (let
      (
        (outcome-total (default-to { total-amount: u0, participants: u0 } (map-get? outcome-totals { bet-id: bet-id, outcome: outcome })))
      )
      (map-set outcome-totals
        { bet-id: bet-id, outcome: outcome }
        {
          total-amount: (+ (get total-amount outcome-total) amount),
          participants: (+ (get participants outcome-total) u1)
        }
      )
    )
    
    ;; Update bet info
    (map-set bets
      { id: bet-id }
      (merge bet {
        total-amount: (+ (get total-amount bet) amount),
        current-participants: (+ (get current-participants bet) u1),
        is-closed: (or 
                    (and (get is-peer-to-peer bet) (is-eq (+ (get current-participants bet) u1) u2))
                    (is-eq (+ (get current-participants bet) u1) (get max-participants bet))
                   )
      })
    )
    
    ;; In a real implementation, you would transfer tokens here
    ;; (contract-call? .token-contract transfer amount tx-sender (as-contract tx-sender))
    
    (ok true)
  )
)

;; Close a bet (only creator can close it before it's full)
(define-public (close-bet (bet-id uint))
  (let
    (
      (bet (unwrap! (map-get? bets { id: bet-id }) (err err-bet-not-found)))
    )
    (asserts! (is-eq tx-sender (get creator bet)) (err err-not-authorized))
    (asserts! (not (get is-closed bet)) (err err-bet-closed))
    
    (map-set bets
      { id: bet-id }
      (merge bet { is-closed: true })
    )
    
    (ok true)
  )
)

;; Settle a bet (only creator can settle)
(define-public (settle-bet (bet-id uint) (winning-outcome (string-ascii 64)))
  (let
    (
      (bet (unwrap! (map-get? bets { id: bet-id }) (err err-bet-not-found)))
    )
    (asserts! (is-eq tx-sender (get creator bet)) (err err-not-authorized))
    (asserts! (get is-closed bet) (err err-bet-not-closed))
    (asserts! (not (get is-settled bet)) (err err-bet-already-settled))
    (asserts! (is-some (index-of (get possible-outcomes bet) winning-outcome)) (err err-invalid-outcome))
    
    (map-set bets
      { id: bet-id }
      (merge bet { 
        is-settled: true,
        winning-outcome: (some winning-outcome)
      })
    )
    
    ;; In a real implementation, you would distribute winnings here
    
    (ok true)
  )
)

;; Claim winnings
(define-public (claim-winnings (bet-id uint))
  (let
    (
      (bet (unwrap! (map-get? bets { id: bet-id }) (err err-bet-not-found)))
      (participant-bet (unwrap! (map-get? bet-participants { bet-id: bet-id, participant: tx-sender }) (err err-not-participant)))
    )
    (asserts! (get is-settled bet) (err err-bet-not-closed))
    (asserts! (is-eq (get outcome participant-bet) (unwrap! (get winning-outcome bet) (err err-bet-not-closed))) (err err-not-participant))
    
    (let
      (
        (winning-outcome-data (unwrap! (map-get? outcome-totals { bet-id: bet-id, outcome: (unwrap-panic (get winning-outcome bet)) }) (err err-invalid-outcome)))
        (winnings (calculate-winnings bet-id tx-sender))
      )
      ;; In a real implementation, you would transfer tokens here
      ;; (contract-call? .token-contract transfer winnings (as-contract tx-sender) tx-sender)
      
      (ok winnings)
    )
  )
)

;; Calculate winnings for a participant
(define-private (calculate-winnings (bet-id uint) (participant principal))
  (let
    (
      (bet (unwrap-panic (map-get? bets { id: bet-id })))
      (participant-bet (unwrap-panic (map-get? bet-participants { bet-id: bet-id, participant: participant })))
      (winning-outcome (unwrap-panic (get winning-outcome bet)))
      (winning-outcome-data (unwrap-panic (map-get? outcome-totals { bet-id: bet-id, outcome: winning-outcome })))
      (participant-share (/ (* (get amount participant-bet) u100) (get total-amount winning-outcome-data)))
    )
    (/ (* (get total-amount bet) participant-share) u100)
  )
)

;; Get bet details
(define-read-only (get-bet (bet-id uint))
  (map-get? bets { id: bet-id })
)

;; Get participant bet details
(define-read-only (get-participant-bet (bet-id uint) (participant principal))
  (map-get? bet-participants { bet-id: bet-id, participant: participant })
)

;; Get outcome totals
(define-read-only (get-outcome-total (bet-id uint) (outcome (string-ascii 64)))
  (map-get? outcome-totals { bet-id: bet-id, outcome: outcome })
)

;; Get total number of bets
(define-read-only (get-bet-count)
  (var-get bet-counter)
)

;; Check if a bet is settled
(define-read-only (is-bet-settled (bet-id uint))
  (default-to false (get is-settled (map-get? bets { id: bet-id })))
)

;; Check if a participant has won a bet
(define-read-only (has-won (bet-id uint) (participant principal))
  (let
    (
      (bet (map-get? bets { id: bet-id }))
      (participant-bet (map-get? bet-participants { bet-id: bet-id, participant: participant }))
    )
    (and
      (is-some bet)
      (is-some participant-bet)
      (get is-settled (unwrap-panic bet))
      (is-some (get winning-outcome (unwrap-panic bet)))
      (is-eq (get outcome (unwrap-panic participant-bet)) (unwrap-panic (get winning-outcome (unwrap-panic bet))))
    )
  )
)