
;; Token Burn Contract
;; Deflationary token burning mechanism for losing bets

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-BURN-RATE (err u103))
(define-constant ERR-UNAUTHORIZED (err u104))
(define-constant ERR-TOO-EARLY (err u105))

;; Added block-height based constants for time calculations
(define-constant BLOCKS-PER-DAY u144) ;; Approximately 144 blocks per day (10 min blocks)
(define-constant BLOCKS-PER-WEEK u1008) ;; 7 days worth of blocks
(define-constant BURN-COOLDOWN-BLOCKS u10) ;; Minimum blocks between burns

;; Data Variables
(define-data-var total-burned uint u0)
(define-data-var current-burn-rate uint u1000) ;; 10% (1000 basis points)
(define-data-var burn-multiplier uint u100) ;; Base multiplier for calculations
;; Added block-height based time tracking
(define-data-var contract-start-block uint u0)
(define-data-var last-burn-block uint u0)
(define-data-var time-based-burn-rate uint u50) ;; Additional burn rate based on time

;; Data Maps
(define-map burn-history 
  { block-number: uint }
  { 
    amount: uint,
    ;; Using block-height instead of timestamp for time tracking
    block-time: uint,
    burn-type: (string-ascii 20),
    initiator: principal
  }
)

(define-map user-burn-totals
  { user: principal }
  { total-burned: uint }
)

(define-map authorized-burners
  { burner: principal }
  { authorized: bool }
)

;; Authorization Functions
(define-public (add-authorized-burner (burner principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (ok (map-set authorized-burners { burner: burner } { authorized: true }))
  )
)

(define-public (remove-authorized-burner (burner principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (ok (map-set authorized-burners { burner: burner } { authorized: false }))
  )
)

(define-read-only (is-authorized-burner (burner principal))
  (default-to false (get authorized (map-get? authorized-burners { burner: burner })))
)

;; Core Burning Functions
(define-public (burn-losing-bet-tokens (amount uint))
  (let (
    (sender tx-sender)
    (current-block block-height)
    ;; Added time-based burn rate calculation
    (time-multiplier (calculate-time-based-multiplier))
    (adjusted-amount (/ (* amount time-multiplier) u100))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (or (is-eq sender CONTRACT-OWNER) (is-authorized-burner sender)) ERR-UNAUTHORIZED)
    ;; Added cooldown check using block-height
    (asserts! (>= (get-blocks-since-last-burn) BURN-COOLDOWN-BLOCKS) ERR-TOO-EARLY)
    
    ;; Record the burn in history
    (map-set burn-history 
      { block-number: current-block }
      {
        amount: adjusted-amount,
        ;; Using block-height for time tracking
        block-time: current-block,
        burn-type: "losing-bet",
        initiator: sender
      }
    )
    
    ;; Update last burn block
    (var-set last-burn-block current-block)
    
    ;; Update user burn totals
    (let (
      (current-user-total (default-to u0 (get total-burned (map-get? user-burn-totals { user: sender }))))
    )
      (map-set user-burn-totals 
        { user: sender }
        { total-burned: (+ current-user-total adjusted-amount) }
      )
    )
    
    ;; Update total burned
    (var-set total-burned (+ (var-get total-burned) adjusted-amount))
    
    ;; Emit burn event (using print for event emission)
    (print {
      event: "token-burn",
      amount: adjusted-amount,
      original-amount: amount,
      time-multiplier: time-multiplier,
      burn-type: "losing-bet",
      total-burned: (var-get total-burned),
      block-number: current-block,
      burner: sender
    })
    
    (ok adjusted-amount)
  )
)

(define-public (calculate-burn-amount (bet-amount uint))
  (let (
    (burn-rate (var-get current-burn-rate))
    (multiplier (var-get burn-multiplier))
    ;; Include time-based multiplier in calculation
    (time-multiplier (calculate-time-based-multiplier))
    (base-burn (/ (* bet-amount burn-rate) (* multiplier u100)))
  )
    (asserts! (> bet-amount u0) ERR-INVALID-AMOUNT)
    (ok (/ (* base-burn time-multiplier) u100))
  )
)

;; Added automatic time-based burn function
(define-public (trigger-time-based-burn)
  (let (
    (current-block block-height)
    (blocks-since-last (get-blocks-since-last-burn))
    (auto-burn-amount (/ (var-get total-burned) u1000)) ;; 0.1% of total burned
  )
    ;; Only allow time-based burn once per day
    (asserts! (>= blocks-since-last BLOCKS-PER-DAY) ERR-TOO-EARLY)
    (asserts! (> auto-burn-amount u0) ERR-INVALID-AMOUNT)
    
    ;; Record automatic burn
    (map-set burn-history 
      { block-number: current-block }
      {
        amount: auto-burn-amount,
        block-time: current-block,
        burn-type: "time-based",
        initiator: tx-sender
      }
    )
    
    (var-set total-burned (+ (var-get total-burned) auto-burn-amount))
    (var-set last-burn-block current-block)
    
    (print {
      event: "time-based-burn",
      amount: auto-burn-amount,
      blocks-since-last: blocks-since-last,
      total-burned: (var-get total-burned),
      block-number: current-block
    })
    
    (ok auto-burn-amount)
  )
)

;; Burn Rate Management
(define-public (update-burn-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= new-rate u10000) ERR-INVALID-BURN-RATE) ;; Max 100%
    (var-set current-burn-rate new-rate)
    (print {
      event: "burn-rate-updated",
      old-rate: (var-get current-burn-rate),
      new-rate: new-rate,
      updated-by: tx-sender
    })
    (ok new-rate)
  )
)

(define-read-only (get-current-burn-rate)
  (ok (var-get current-burn-rate))
)

;; Burn History and Statistics
(define-read-only (get-total-burned)
  (ok (var-get total-burned))
)

(define-read-only (get-burn-history (block-number uint))
  (map-get? burn-history { block-number: block-number })
)

(define-read-only (get-user-burn-total (user principal))
  (default-to u0 (get total-burned (map-get? user-burn-totals { user: user })))
)

;; Time-based Burn Calculation Functions
(define-read-only (get-blocks-since-start)
  (- block-height (var-get contract-start-block))
)

(define-read-only (get-blocks-since-last-burn)
  (- block-height (var-get last-burn-block))
)

(define-read-only (calculate-time-based-multiplier)
  (let (
    (blocks-elapsed (get-blocks-since-start))
    (weeks-elapsed (/ blocks-elapsed BLOCKS-PER-WEEK))
  )
    ;; Increase burn rate by 5% every week (max 200% after 20 weeks)
    (if (> weeks-elapsed u20)
      u200
      (+ u100 (* weeks-elapsed u5))
    )
  )
)

;; Utility Functions
(define-read-only (get-burn-statistics)
  (ok {
    total-burned: (var-get total-burned),
    current-burn-rate: (var-get current-burn-rate),
    burn-multiplier: (var-get burn-multiplier),
    blocks-since-start: (get-blocks-since-start),
    blocks-since-last-burn: (get-blocks-since-last-burn),
    time-multiplier: (calculate-time-based-multiplier),
    contract-start-block: (var-get contract-start-block)
  })
)

(define-read-only (can-trigger-time-burn)
  (>= (get-blocks-since-last-burn) BLOCKS-PER-DAY)
)

;; Initialize contract
(begin
  ;; Set contract start block for time-based calculations
  (var-set contract-start-block block-height)
  (var-set last-burn-block block-height)
  (map-set authorized-burners { burner: CONTRACT-OWNER } { authorized: true })
  (print { 
    event: "contract-initialized", 
    owner: CONTRACT-OWNER,
    start-block: block-height
  })
)

