;; betting-engine.clar
;; Main betting logic and bet placement system for Wagerr platform

;; ===== CONSTANTS =====
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_INVALID_BET (err u402))
(define-constant ERR_INSUFFICIENT_BALANCE (err u403))
(define-constant ERR_EVENT_NOT_FOUND (err u404))
(define-constant ERR_EVENT_CLOSED (err u405))
(define-constant ERR_BET_NOT_FOUND (err u406))
(define-constant ERR_BET_ALREADY_SETTLED (err u407))
(define-constant ERR_INVALID_ODDS (err u408))
(define-constant ERR_AMOUNT_TOO_LOW (err u409))
(define-constant ERR_AMOUNT_TOO_HIGH (err u410))
(define-constant ERR_BETTING_CLOSED (err u411))
(define-constant ERR_INVALID_SELECTION (err u412))

;; Betting limits
(define-constant MIN_BET_AMOUNT u100000000) ;; 1 WGR (8 decimals)
(define-constant MAX_BET_AMOUNT u100000000000000) ;; 1M WGR
(define-constant MIN_ODDS u10000) ;; 1.0001 (4 decimals for odds)
(define-constant MAX_ODDS u1000000) ;; 100.0000

;; Bet status constants
(define-constant BET_STATUS_ACTIVE u1)
(define-constant BET_STATUS_WON u2)
(define-constant BET_STATUS_LOST u3)
(define-constant BET_STATUS_CANCELLED u4)
(define-constant BET_STATUS_PUSHED u5) ;; Tie/Draw

;; Event status constants
(define-constant EVENT_STATUS_OPEN u1)
(define-constant EVENT_STATUS_CLOSED u2)
(define-constant EVENT_STATUS_SETTLED u3)
(define-constant EVENT_STATUS_CANCELLED u4)

;; ===== DATA VARIABLES =====
(define-data-var next-bet-id uint u1)
(define-data-var platform-enabled bool true)
(define-data-var total-volume uint u0)
(define-data-var total-bets-placed uint u0)

;; ===== DATA MAPS =====
;; Bet information storage
(define-map bets uint {
    bet-id: uint,
    user: principal,
    event-id: uint,
    selection: uint,
    amount: uint,
    odds: uint,
    potential-payout: uint,
    status: uint,
    timestamp: uint,
    settlement-timestamp: (optional uint),
    result: (optional uint)
})

;; User bet tracking
(define-map user-bet-ids principal (list 1000 uint))

;; Event information
(define-map events uint {
    event-id: uint,
    name: (string-ascii 128),
    category: (string-ascii 64),
    start-time: uint,
    end-time: uint,
    status: uint,
    total-volume: uint,
    selections: (list 10 (string-ascii 64)),
    result: (optional uint),
    created-by: principal
})

;; Event betting statistics
(define-map event-stats uint {
    total-bets: uint,
    total-volume: uint,
    selection-volumes: (list 10 uint)
})

;; User statistics
(define-map user-stats principal {
    total-bets: uint,
    total-volume: uint,
    wins: uint,
    losses: uint,
    total-winnings: uint
})

;; Betting limits per user
(define-map user-limits principal {
    daily-limit: uint,
    daily-spent: uint,
    last-reset: uint
})

;; ===== AUTHORIZATION FUNCTIONS =====
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT_OWNER))

(define-private (is-platform-enabled)
    (var-get platform-enabled))

;; ===== UTILITY FUNCTIONS =====
(define-private (get-current-time)
    stacks-block-height) ;; Using block height as time proxy

(define-private (calculate-payout-amount (amount uint) (odds uint))
    (/ (* amount odds) u10000)) ;; odds are in 4 decimal format

(define-private (is-valid-principal (user principal))
    (not (is-eq user CONTRACT_OWNER)))

;; ===== VALIDATION FUNCTIONS =====
(define-private (validate-bet-parameters (event-id uint) (selection uint) (amount uint) (odds uint))
    (and
        ;; Check platform is enabled
        (is-platform-enabled)
        ;; Check amount limits
        (>= amount MIN_BET_AMOUNT)
        (<= amount MAX_BET_AMOUNT)
        ;; Check odds limits
        (>= odds MIN_ODDS)
        (<= odds MAX_ODDS)
        ;; Check selection is valid (non-zero)
        (> selection u0)
        ;; Check event exists and is valid
        (is-some (map-get? events event-id))
    ))

(define-private (check-betting-limits (user principal) (amount uint))
    (let (
        (limits (default-to 
            {daily-limit: u10000000000000, daily-spent: u0, last-reset: u0} 
            (map-get? user-limits user)))
        (current-time (get-current-time))
        (daily-reset-needed (> (- current-time (get last-reset limits)) u144)) ;; ~24 hours in blocks
    )
    (let (
        (updated-limits (if daily-reset-needed
            (merge limits {daily-spent: u0, last-reset: current-time})
            limits))
        (new-daily-spent (+ (get daily-spent updated-limits) amount))
    )
    (and
        (<= new-daily-spent (get daily-limit updated-limits))
        ;; Update the limits if validation passes
        (begin
            (map-set user-limits user (merge updated-limits {daily-spent: new-daily-spent}))
            true
        )
    ))))

(define-private (verify-event-status (event-id uint))
    (match (map-get? events event-id)
        event-data (and
            (is-eq (get status event-data) EVENT_STATUS_OPEN)
            (> (get start-time event-data) (get-current-time))
        )
        false
    ))

(define-private (validate-selection (event-id uint) (selection uint))
    (match (map-get? events event-id)
        event-data (let (
            (selections-list (get selections event-data))
            (selections-count (len selections-list))
        )
        (and (> selection u0) (<= selection selections-count)))
        false
    ))

;; ===== CORE BETTING FUNCTIONS =====
(define-public (place-bet (event-id uint) (selection uint) (amount uint) (odds uint))
    (let (
        (bet-id (var-get next-bet-id))
        (user tx-sender)
        (current-time (get-current-time))
        (potential-payout (calculate-payout-amount amount odds))
    )
    (asserts! (is-valid-principal user) ERR_UNAUTHORIZED)
    (asserts! (validate-bet-parameters event-id selection amount odds) ERR_INVALID_BET)
    (asserts! (verify-event-status event-id) ERR_EVENT_CLOSED)
    (asserts! (validate-selection event-id selection) ERR_INVALID_SELECTION)
    (asserts! (check-betting-limits user amount) ERR_AMOUNT_TOO_HIGH)
    
    ;; Store bet information
    (map-set bets bet-id {
        bet-id: bet-id,
        user: user,
        event-id: event-id,
        selection: selection,
        amount: amount,
        odds: odds,
        potential-payout: potential-payout,
        status: BET_STATUS_ACTIVE,
        timestamp: current-time,
        settlement-timestamp: none,
        result: none
    })
    
    ;; Update user bet tracking
    (let (
        (current-user-bets (default-to (list) (map-get? user-bet-ids user)))
        (updated-user-bets (unwrap! (as-max-len? (append current-user-bets bet-id) u1000) ERR_INVALID_BET))
    )
    (map-set user-bet-ids user updated-user-bets))
    
    ;; Update statistics
    (update-user-stats user amount)
    (update-event-stats event-id selection amount)
    
    ;; Update global counters
    (var-set next-bet-id (+ bet-id u1))
    (var-set total-volume (+ (var-get total-volume) amount))
    (var-set total-bets-placed (+ (var-get total-bets-placed) u1))
    
    (ok bet-id)
    ))

(define-public (cancel-bet (bet-id uint))
    (let (
        (bet-data (unwrap! (map-get? bets bet-id) ERR_BET_NOT_FOUND))
        (user tx-sender)
    )
    (asserts! (is-eq (get user bet-data) user) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status bet-data) BET_STATUS_ACTIVE) ERR_BET_ALREADY_SETTLED)
    
    ;; Check if event is still open for cancellation (before start time)
    (let (
        (event-data (unwrap! (map-get? events (get event-id bet-data)) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (> (get start-time event-data) (get-current-time)) ERR_BETTING_CLOSED)
    
    ;; Update bet status to cancelled
    (map-set bets bet-id (merge bet-data {
        status: BET_STATUS_CANCELLED,
        settlement-timestamp: (some (get-current-time))
    }))
    
    (ok true)
    )))

(define-read-only (get-bet-details (bet-id uint))
    (map-get? bets bet-id))

(define-read-only (get-user-bets (user principal))
    (let (
        (user-bet-list (default-to (list) (map-get? user-bet-ids user)))
    )
    (map get-bet-details user-bet-list)))

(define-read-only (calculate-payout (bet-id uint))
    (match (map-get? bets bet-id)
        bet-data (let (
            (amount (get amount bet-data))
            (odds (get odds bet-data))
            (status (get status bet-data))
        )
        (if (is-eq status BET_STATUS_WON)
            (some (calculate-payout-amount amount odds))
            (if (is-eq status BET_STATUS_PUSHED)
                (some amount) ;; Return original bet amount for pushes
                (some u0) ;; Lost or cancelled bets
            )
        ))
        none
    ))

;; ===== EVENT MANAGEMENT FUNCTIONS =====
(define-public (create-event (name (string-ascii 128)) (category (string-ascii 64)) 
                           (start-time uint) (end-time uint) (selections (list 10 (string-ascii 64))))
    (let (
        (event-id (+ (var-get next-bet-id) u1000000)) ;; Offset to avoid collision with bet IDs
        (current-time (get-current-time))
    )
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (asserts! (> start-time current-time) ERR_INVALID_BET)
    (asserts! (> end-time start-time) ERR_INVALID_BET)
    (asserts! (> (len selections) u1) ERR_INVALID_BET)
    
    (map-set events event-id {
        event-id: event-id,
        name: name,
        category: category,
        start-time: start-time,
        end-time: end-time,
        status: EVENT_STATUS_OPEN,
        total-volume: u0,
        selections: selections,
        result: none,
        created-by: tx-sender
    })
    
    ;; Initialize event stats
    (map-set event-stats event-id {
        total-bets: u0,
        total-volume: u0,
        selection-volumes: (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0)
    })
    
    (ok event-id)
    ))

(define-public (close-event-betting (event-id uint))
    (let (
        (event-data (unwrap! (map-get? events event-id) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status event-data) EVENT_STATUS_OPEN) ERR_EVENT_CLOSED)
    
    (map-set events event-id (merge event-data {status: EVENT_STATUS_CLOSED}))
    (ok true)
    ))

(define-public (settle-event (event-id uint) (winning-selection uint))
    (let (
        (event-data (unwrap! (map-get? events event-id) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status event-data) EVENT_STATUS_CLOSED) ERR_EVENT_NOT_FOUND)
    (asserts! (validate-selection event-id winning-selection) ERR_INVALID_SELECTION)
    
    ;; Update event with result
    (map-set events event-id (merge event-data {
        status: EVENT_STATUS_SETTLED,
        result: (some winning-selection)
    }))
    
    (ok true)
    ))

;; ===== STATISTICS UPDATE FUNCTIONS =====
(define-private (update-user-stats (user principal) (amount uint))
    (let (
        (current-stats (default-to 
            {total-bets: u0, total-volume: u0, wins: u0, losses: u0, total-winnings: u0}
            (map-get? user-stats user)))
        (updated-stats (merge current-stats {
            total-bets: (+ (get total-bets current-stats) u1),
            total-volume: (+ (get total-volume current-stats) amount)
        }))
    )
    (map-set user-stats user updated-stats)
    ))

(define-private (update-event-stats (event-id uint) (selection uint) (amount uint))
    (let (
        (current-stats (default-to 
            {total-bets: u0, total-volume: u0, selection-volumes: (list u0 u0 u0 u0 u0 u0 u0 u0 u0 u0)}
            (map-get? event-stats event-id)))
        (updated-stats (merge current-stats {
            total-bets: (+ (get total-bets current-stats) u1),
            total-volume: (+ (get total-volume current-stats) amount)
        }))
    )
    (map-set event-stats event-id updated-stats)
    ))

;; ===== QUERY FUNCTIONS =====
(define-read-only (get-event-details (event-id uint))
    (map-get? events event-id))

(define-read-only (get-event-stats (event-id uint))
    (map-get? event-stats event-id))

(define-read-only (get-user-stats (user principal))
    (map-get? user-stats user))

(define-read-only (get-platform-stats)
    {
        total-volume: (var-get total-volume),
        total-bets: (var-get total-bets-placed),
        platform-enabled: (var-get platform-enabled)
    })

(define-read-only (get-active-events)
    ;; This would need to be implemented with a more sophisticated query system
    ;; For now, returning a simple indicator
    (ok "Use event-manager contract for comprehensive event queries"))

;; ===== ADMIN FUNCTIONS =====
(define-public (toggle-platform (enabled bool))
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (var-set platform-enabled enabled)
        (ok enabled)
    ))

(define-public (update-bet-status (bet-id uint) (new-status uint) (result (optional uint)))
    (let (
        (bet-data (unwrap! (map-get? bets bet-id) ERR_BET_NOT_FOUND))
    )
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    
    (map-set bets bet-id (merge bet-data {
        status: new-status,
        settlement-timestamp: (some (get-current-time)),
        result: result
    }))
    
    ;; Update user statistics based on result
    (if (is-eq new-status BET_STATUS_WON)
        (update-user-win-stats (get user bet-data) (get potential-payout bet-data))
        (if (is-eq new-status BET_STATUS_LOST)
            (update-user-loss-stats (get user bet-data))
            true
        )
    )
    
    (ok true)
    ))

(define-private (update-user-win-stats (user principal) (winnings uint))
    (let (
        (current-stats (default-to 
            {total-bets: u0, total-volume: u0, wins: u0, losses: u0, total-winnings: u0}
            (map-get? user-stats user)))
        (updated-stats (merge current-stats {
            wins: (+ (get wins current-stats) u1),
            total-winnings: (+ (get total-winnings current-stats) winnings)
        }))
    )
    (map-set user-stats user updated-stats)
    ))

(define-private (update-user-loss-stats (user principal))
    (let (
        (current-stats (default-to 
            {total-bets: u0, total-volume: u0, wins: u0, losses: u0, total-winnings: u0}
            (map-get? user-stats user)))
        (updated-stats (merge current-stats {
            losses: (+ (get losses current-stats) u1)
        }))
    )
    (map-set user-stats user updated-stats)
    ))

;; ===== EMERGENCY FUNCTIONS =====
(define-public (emergency-cancel-bet (bet-id uint))
    (let (
        (bet-data (unwrap! (map-get? bets bet-id) ERR_BET_NOT_FOUND))
    )
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status bet-data) BET_STATUS_ACTIVE) ERR_BET_ALREADY_SETTLED)
    
    (map-set bets bet-id (merge bet-data {
        status: BET_STATUS_CANCELLED,
        settlement-timestamp: (some (get-current-time))
    }))
    
    (ok true)
    ))

;; ===== INTEGRATION FUNCTIONS =====
;; These functions would be called by other contracts (oracle, payout processor, etc.)
(define-public (bulk-settle-bets (bet-results (list 100 {bet-id: uint, status: uint, result: (optional uint)})))
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (ok (map settle-single-bet bet-results))
    ))

(define-private (settle-single-bet (bet-result {bet-id: uint, status: uint, result: (optional uint)}))
    (update-bet-status (get bet-id bet-result) (get status bet-result) (get result bet-result)))

;; Initialize contract
(begin
    (var-set next-bet-id u1)
    (var-set platform-enabled true)
    (var-set total-volume u0)
    (var-set total-bets-placed u0)
)