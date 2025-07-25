;; Event Manager Smart Contract
;; Manages sports events and betting markets

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_EVENT_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_BETTING_CLOSED (err u103))
(define-constant ERR_MARKET_NOT_FOUND (err u104))
(define-constant ERR_INVALID_ODDS (err u105))
(define-constant ERR_EVENT_ALREADY_EXISTS (err u106))
(define-constant ERR_INVALID_MARKET_TYPE (err u107))

;; Event Status Constants
(define-constant STATUS_CREATED u0)
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_SUSPENDED u2)
(define-constant STATUS_COMPLETED u3)
(define-constant STATUS_CANCELLED u4)

;; Market Type Constants
(define-constant MARKET_WINNER u0)
(define-constant MARKET_OVER_UNDER u1)
(define-constant MARKET_HANDICAP u2)
(define-constant MARKET_PROP_BET u3)

;; Sport Category Constants
(define-constant SPORT_FOOTBALL u0)
(define-constant SPORT_BASKETBALL u1)
(define-constant SPORT_BASEBALL u2)
(define-constant SPORT_SOCCER u3)
(define-constant SPORT_TENNIS u4)
(define-constant SPORT_ESPORTS u5)

;; Data Variables
(define-data-var next-event-id uint u1)
(define-data-var next-market-id uint u1)

;; Data Maps
(define-map events
  { event-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    sport-category: uint,
    start-block: uint,
    end-block: (optional uint),
    status: uint,
    home-team: (string-ascii 50),
    away-team: (string-ascii 50),
    venue: (string-ascii 100),
    creator: principal,
    created-block: uint,
    betting-closed: bool,
    result: (optional (string-ascii 100))
  }
)

(define-map betting-markets
  { market-id: uint }
  {
    event-id: uint,
    market-type: uint,
    name: (string-ascii 100),
    description: (string-ascii 200),
    odds: (list 10 uint),
    outcomes: (list 10 (string-ascii 50)),
    is-active: bool,
    created-block: uint,
    suspended: bool,
    min-bet: uint,
    max-bet: uint
  }
)

(define-map event-markets
  { event-id: uint }
  { market-ids: (list 20 uint) }
)

(define-map oracle-feeds
  { event-id: uint }
  {
    feed-url: (string-ascii 200),
    last-update-block: uint,
    is-active: bool
  }
)

(define-map authorized-oracles
  { oracle: principal }
  { authorized: bool }
)

;; Event Management Functions

(define-public (create-event (event-data (tuple 
  (name (string-ascii 100))
  (description (string-ascii 500))
  (sport-category uint)
  (start-block uint)
  (home-team (string-ascii 50))
  (away-team (string-ascii 50))
  (venue (string-ascii 100)))))
  (let
    (
      (event-id (var-get next-event-id))
    )
    (asserts! (> (get start-block event-data) stacks-block-height) ERR_INVALID_STATUS)
    (asserts! (<= (get sport-category event-data) SPORT_ESPORTS) ERR_INVALID_STATUS)
    
    ;; Create the event
    (map-set events
      { event-id: event-id }
      {
        name: (get name event-data),
        description: (get description event-data),
        sport-category: (get sport-category event-data),
        start-block: (get start-block event-data),
        end-block: none,
        status: STATUS_CREATED,
        home-team: (get home-team event-data),
        away-team: (get away-team event-data),
        venue: (get venue event-data),
        creator: tx-sender,
        created-block: stacks-block-height,
        betting-closed: false,
        result: none
      }
    )
    
    ;; Initialize empty market list for this event
    (map-set event-markets
      { event-id: event-id }
      { market-ids: (list) }
    )
    
    ;; Increment event ID counter
    (var-set next-event-id (+ event-id u1))
    
    (print { event: "event-created", event-id: event-id, creator: tx-sender })
    (ok event-id)
  )
)

(define-public (update-event-status (event-id uint) (status uint))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get creator event))) ERR_UNAUTHORIZED)
    (asserts! (<= status STATUS_CANCELLED) ERR_INVALID_STATUS)
    
    (map-set events
      { event-id: event-id }
      (merge event { status: status })
    )
    
    ;; If event is completed or cancelled, close betting
    (if (or (is-eq status STATUS_COMPLETED) (is-eq status STATUS_CANCELLED))
      (map-set events
        { event-id: event-id }
        (merge event { status: status, betting-closed: true })
      )
      true
    )
    
    (print { event: "event-status-updated", event-id: event-id, new-status: status })
    (ok true)
  )
)

(define-public (close-betting (event-id uint))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get creator event))) ERR_UNAUTHORIZED)
    (asserts! (not (get betting-closed event)) ERR_BETTING_CLOSED)
    
    (map-set events
      { event-id: event-id }
      (merge event { betting-closed: true })
    )
    
    (print { event: "betting-closed", event-id: event-id })
    (ok true)
  )
)

(define-public (set-event-result (event-id uint) (result (string-ascii 100)))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (get authorized (default-to { authorized: false } (map-get? authorized-oracles { oracle: tx-sender })))) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status event) STATUS_COMPLETED) ERR_INVALID_STATUS)
    
    (map-set events
      { event-id: event-id }
      (merge event { result: (some result) })
    )
    
    (print { event: "event-result-set", event-id: event-id, result: result })
    (ok true)
  )
)

;; Market Management Functions

(define-public (create-betting-market (event-id uint) (market-data (tuple
  (market-type uint)
  (name (string-ascii 100))
  (description (string-ascii 200))
  (odds (list 10 uint))
  (outcomes (list 10 (string-ascii 50)))
  (min-bet uint)
  (max-bet uint))))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR_EVENT_NOT_FOUND))
      (market-id (var-get next-market-id))
      (current-markets (default-to { market-ids: (list) } (map-get? event-markets { event-id: event-id })))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get creator event))) ERR_UNAUTHORIZED)
    (asserts! (not (get betting-closed event)) ERR_BETTING_CLOSED)
    (asserts! (<= (get market-type market-data) MARKET_PROP_BET) ERR_INVALID_MARKET_TYPE)
    (asserts! (> (len (get odds market-data)) u0) ERR_INVALID_ODDS)
    (asserts! (is-eq (len (get odds market-data)) (len (get outcomes market-data))) ERR_INVALID_ODDS)
    
    ;; Create the betting market
    (map-set betting-markets
      { market-id: market-id }
      {
        event-id: event-id,
        market-type: (get market-type market-data),
        name: (get name market-data),
        description: (get description market-data),
        odds: (get odds market-data),
        outcomes: (get outcomes market-data),
        is-active: true,
        created-block: stacks-block-height,
        suspended: false,
        min-bet: (get min-bet market-data),
        max-bet: (get max-bet market-data)
      }
    )
    
    ;; Add market to event's market list
    (map-set event-markets
      { event-id: event-id }
      { market-ids: (unwrap-panic (as-max-len? (append (get market-ids current-markets) market-id) u20)) }
    )
    
    ;; Increment market ID counter
    (var-set next-market-id (+ market-id u1))
    
    (print { event: "market-created", market-id: market-id, event-id: event-id })
    (ok market-id)
  )
)

(define-public (update-odds (market-id uint) (new-odds (list 10 uint)))
  (let
    (
      (market (unwrap! (map-get? betting-markets { market-id: market-id }) ERR_MARKET_NOT_FOUND))
      (event (unwrap! (map-get? events { event-id: (get event-id market) }) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get creator event))) ERR_UNAUTHORIZED)
    (asserts! (get is-active market) ERR_MARKET_NOT_FOUND)
    (asserts! (not (get suspended market)) ERR_MARKET_NOT_FOUND)
    (asserts! (is-eq (len new-odds) (len (get outcomes market))) ERR_INVALID_ODDS)
    
    (map-set betting-markets
      { market-id: market-id }
      (merge market { odds: new-odds })
    )
    
    (print { event: "odds-updated", market-id: market-id, new-odds: new-odds })
    (ok true)
  )
)

(define-public (suspend-market (market-id uint))
  (let
    (
      (market (unwrap! (map-get? betting-markets { market-id: market-id }) ERR_MARKET_NOT_FOUND))
      (event (unwrap! (map-get? events { event-id: (get event-id market) }) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get creator event))) ERR_UNAUTHORIZED)
    
    (map-set betting-markets
      { market-id: market-id }
      (merge market { suspended: true })
    )
    
    (print { event: "market-suspended", market-id: market-id })
    (ok true)
  )
)

(define-public (resume-market (market-id uint))
  (let
    (
      (market (unwrap! (map-get? betting-markets { market-id: market-id }) ERR_MARKET_NOT_FOUND))
      (event (unwrap! (map-get? events { event-id: (get event-id market) }) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get creator event))) ERR_UNAUTHORIZED)
    
    (map-set betting-markets
      { market-id: market-id }
      (merge market { suspended: false })
    )
    
    (print { event: "market-resumed", market-id: market-id })
    (ok true)
  )
)

;; Oracle Management Functions

(define-public (authorize-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-oracles
      { oracle: oracle }
      { authorized: true }
    )
    (print { event: "oracle-authorized", oracle: oracle })
    (ok true)
  )
)

(define-public (revoke-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-oracles
      { oracle: oracle }
      { authorized: false }
    )
    (print { event: "oracle-revoked", oracle: oracle })
    (ok true)
  )
)

(define-public (set-oracle-feed (event-id uint) (feed-url (string-ascii 200)))
  (let
    (
      (event (unwrap! (map-get? events { event-id: event-id }) ERR_EVENT_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get creator event))) ERR_UNAUTHORIZED)
    
    (map-set oracle-feeds
      { event-id: event-id }
      {
        feed-url: feed-url,
        last-update-block: stacks-block-height,
        is-active: true
      }
    )
    
    (print { event: "oracle-feed-set", event-id: event-id, feed-url: feed-url })
    (ok true)
  )
)

;; Read-only Functions - Simplified without complex list operations

(define-read-only (get-event-details (event-id uint))
  (ok (map-get? events { event-id: event-id }))
)

(define-read-only (get-market-details (market-id uint))
  (ok (map-get? betting-markets { market-id: market-id }))
)

(define-read-only (get-event-markets (event-id uint))
  (ok (map-get? event-markets { event-id: event-id }))
)

(define-read-only (get-oracle-feed (event-id uint))
  (ok (map-get? oracle-feeds { event-id: event-id }))
)

(define-read-only (is-oracle-authorized (oracle principal))
  (get authorized (default-to { authorized: false } (map-get? authorized-oracles { oracle: oracle })))
)

(define-read-only (get-total-events)
  (ok (- (var-get next-event-id) u1))
)

(define-read-only (get-total-markets)
  (ok (- (var-get next-market-id) u1))
)

;; Individual event checking functions - no list operations
(define-read-only (is-event-active (event-id uint))
  (match (map-get? events { event-id: event-id })
    event (ok (is-eq (get status event) STATUS_ACTIVE))
    (ok false)
  )
)

(define-read-only (is-event-upcoming (event-id uint))
  (match (map-get? events { event-id: event-id })
    event (ok (and (> (get start-block event) stacks-block-height)
                   (or (is-eq (get status event) STATUS_CREATED)
                       (is-eq (get status event) STATUS_ACTIVE))))
    (ok false)
  )
)

(define-read-only (get-event-sport-category (event-id uint))
  (match (map-get? events { event-id: event-id })
    event (ok (some (get sport-category event)))
    (ok none)
  )
)

(define-read-only (get-event-status (event-id uint))
  (match (map-get? events { event-id: event-id })
    event (ok (some (get status event)))
    (ok none)
  )
)

(define-read-only (event-exists (event-id uint))
  (is-some (map-get? events { event-id: event-id }))
)

(define-read-only (is-betting-open (event-id uint))
  (match (map-get? events { event-id: event-id })
    event (ok (not (get betting-closed event)))
    (ok false)
  )
)

(define-read-only (get-event-creator (event-id uint))
  (match (map-get? events { event-id: event-id })
    event (ok (some (get creator event)))
    (ok none)
  )
)

(define-read-only (get-market-event-id (market-id uint))
  (match (map-get? betting-markets { market-id: market-id })
    market (ok (some (get event-id market)))
    (ok none)
  )
)

(define-read-only (is-market-active (market-id uint))
  (match (map-get? betting-markets { market-id: market-id })
    market (ok (and (get is-active market) (not (get suspended market))))
    (ok false)
  )
)

(define-read-only (get-market-odds (market-id uint))
  (match (map-get? betting-markets { market-id: market-id })
    market (ok (some (get odds market)))
    (ok none)
  )
)

;; Batch checking functions for multiple events (up to 5 at a time)
(define-read-only (check-events-active (event-ids (list 5 uint)))
  (ok {
    event-1: (if (>= (len event-ids) u1) (unwrap-panic (is-event-active (unwrap-panic (element-at event-ids u0)))) false),
    event-2: (if (>= (len event-ids) u2) (unwrap-panic (is-event-active (unwrap-panic (element-at event-ids u1)))) false),
    event-3: (if (>= (len event-ids) u3) (unwrap-panic (is-event-active (unwrap-panic (element-at event-ids u2)))) false),
    event-4: (if (>= (len event-ids) u4) (unwrap-panic (is-event-active (unwrap-panic (element-at event-ids u3)))) false),
    event-5: (if (>= (len event-ids) u5) (unwrap-panic (is-event-active (unwrap-panic (element-at event-ids u4)))) false)
  })
)

(define-read-only (check-events-upcoming (event-ids (list 5 uint)))
  (ok {
    event-1: (if (>= (len event-ids) u1) (unwrap-panic (is-event-upcoming (unwrap-panic (element-at event-ids u0)))) false),
    event-2: (if (>= (len event-ids) u2) (unwrap-panic (is-event-upcoming (unwrap-panic (element-at event-ids u1)))) false),
    event-3: (if (>= (len event-ids) u3) (unwrap-panic (is-event-upcoming (unwrap-panic (element-at event-ids u2)))) false),
    event-4: (if (>= (len event-ids) u4) (unwrap-panic (is-event-upcoming (unwrap-panic (element-at event-ids u3)))) false),
    event-5: (if (>= (len event-ids) u5) (unwrap-panic (is-event-upcoming (unwrap-panic (element-at event-ids u4)))) false)
  })
)

;; Initialize contract
(begin
  (map-set authorized-oracles
    { oracle: CONTRACT_OWNER }
    { authorized: true }
  )
)
