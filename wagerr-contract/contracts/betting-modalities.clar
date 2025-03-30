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

