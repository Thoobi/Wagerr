;; WGR Token Smart Contract
;; A fungible token implementation in Clarity

;; Define the fungible token
(define-fungible-token wgr-token)

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))

;; Define data variables
(define-data-var token-name (string-ascii 32) "WGR Token")
(define-data-var token-symbol (string-ascii 10) "WGR")
(define-data-var token-decimals uint u6)
(define-data-var total-supply uint u0)

;; SIP-010 trait implementation
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
        (asserts! (or (is-eq tx-sender sender) (is-eq contract-caller sender)) err-not-token-owner)
        (asserts! (> amount u0) err-invalid-amount)
        (ft-transfer? wgr-token amount sender recipient)
    )
)

(define-read-only (get-name)
    (ok (var-get token-name))
)

(define-read-only (get-symbol)
    (ok (var-get token-symbol))
)

(define-read-only (get-decimals)
    (ok (var-get token-decimals))
)

(define-read-only (get-balance (who principal))
    (ok (ft-get-balance wgr-token who))
)

(define-read-only (get-total-supply)
    (ok (ft-get-supply wgr-token))
)

(define-read-only (get-token-uri)
    (ok none)
)

;; Mint function (only contract owner)
(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        (try! (ft-mint? wgr-token amount recipient))
        (var-set total-supply (+ (var-get total-supply) amount))
        (ok true)
    )
)

;; Burn function
(define-public (burn (amount uint) (owner principal))
    (begin
        (asserts! (or (is-eq tx-sender owner) (is-eq contract-caller owner)) err-not-token-owner)
        (asserts! (> amount u0) err-invalid-amount)
        (try! (ft-burn? wgr-token amount owner))
        (var-set total-supply (- (var-get total-supply) amount))
        (ok true)
    )
)

;; Admin functions
(define-public (set-token-uri (uri (optional (string-utf8 256))))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok true)
    )
)

;; Initialize the contract
(begin
    (try! (ft-mint? wgr-token u1000000000000 contract-owner))
    (var-set total-supply u1000000000000)
)