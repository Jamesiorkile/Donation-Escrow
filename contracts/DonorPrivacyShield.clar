;; Donor Privacy Shield - Anonymous donation system with commitment-reveal scheme
;; Allows donors to make anonymous donations with optional identity reveal

;; Error constants
(define-constant err-commitment-exists (err u200))
(define-constant err-commitment-not-found (err u201))
(define-constant err-invalid-reveal (err u202))
(define-constant err-already-revealed (err u203))
(define-constant err-reveal-window-closed (err u204))
(define-constant err-insufficient-amount (err u205))
(define-constant err-campaign-not-found (err u206))
(define-constant err-zero-amount (err u207))

;; Constants
(define-constant min-donation-amount u1000)  ;; Minimum donation in microSTX
(define-constant reveal-window-blocks u2160) ;; 15 days in blocks (approx 24h * 15)

;; Anonymous donation commitment structure
(define-map anonymous-commitments
  { commitment-hash: (buff 32) }
  {
    campaign-id: uint,
    committed-amount: uint,
    commitment-block: uint,
    is-revealed: bool,
    revealed-donor: (optional principal),
    revealed-at: (optional uint)
  }
)

;; Track campaign anonymous donation totals
(define-map campaign-anonymous-totals
  { campaign-id: uint }
  { total-anonymous: uint, commitment-count: uint }
)

;; Privacy preferences for donors
(define-map donor-privacy-settings
  { donor: principal }
  {
    default-anonymous: bool,
    auto-reveal-threshold: uint, ;; Amount above which to auto-reveal (0 = never)
    allow-partial-reveals: bool
  }
)

;; Anonymous donation reveals for recognition
(define-map revealed-donations
  { commitment-hash: (buff 32) }
  {
    donor: principal,
    campaign-id: uint,
    amount: uint,
    reveal-message: (string-ascii 200),
    revealed-at: uint
  }
)

;; Commitment nonce tracking (prevents replay attacks)
(define-map commitment-nonces
  { donor: principal, nonce: uint }
  { used: bool }
)

;; Data variable for commitment statistics
(define-data-var total-anonymous-donations uint u0)

;; Read-only functions

;; Get anonymous commitment details
(define-read-only (get-anonymous-commitment (commitment-hash (buff 32)))
  (map-get? anonymous-commitments { commitment-hash: commitment-hash })
)

;; Get campaign anonymous donation totals
(define-read-only (get-campaign-anonymous-total (campaign-id uint))
  (default-to 
    { total-anonymous: u0, commitment-count: u0 }
    (map-get? campaign-anonymous-totals { campaign-id: campaign-id })
  )
)

;; Get donor privacy settings
(define-read-only (get-donor-privacy-settings (donor principal))
  (default-to 
    { default-anonymous: false, auto-reveal-threshold: u0, allow-partial-reveals: true }
    (map-get? donor-privacy-settings { donor: donor })
  )
)

;; Get revealed donation details
(define-read-only (get-revealed-donation (commitment-hash (buff 32)))
  (map-get? revealed-donations { commitment-hash: commitment-hash })
)

;; Check if commitment is within reveal window
(define-read-only (is-reveal-window-open (commitment-hash (buff 32)))
  (match (get-anonymous-commitment commitment-hash)
    commitment
    (let ((blocks-since-commit (- stacks-block-height (get commitment-block commitment))))
      (<= blocks-since-commit reveal-window-blocks)
    )
    false
  )
)

;; Generate commitment hash (utility function for off-chain use)
(define-read-only (generate-commitment-hash (donor principal) (amount uint) (campaign-id uint) (nonce uint))
  (keccak256 (concat
    (concat (unwrap-panic (to-consensus-buff? donor)) (unwrap-panic (to-consensus-buff? amount)))
    (concat (unwrap-panic (to-consensus-buff? campaign-id)) (unwrap-panic (to-consensus-buff? nonce)))
  ))
)

;; Check if nonce is available
(define-read-only (is-nonce-available (donor principal) (nonce uint))
  (is-none (map-get? commitment-nonces { donor: donor, nonce: nonce }))
)

;; Get total anonymous donations count
(define-read-only (get-total-anonymous-donations)
  (var-get total-anonymous-donations)
)

;; Public functions

;; Set privacy preferences for donor
(define-public (set-privacy-preferences (default-anonymous bool) (auto-reveal-threshold uint) (allow-partial-reveals bool))
  (begin
    (map-set donor-privacy-settings
      { donor: tx-sender }
      {
        default-anonymous: default-anonymous,
        auto-reveal-threshold: auto-reveal-threshold,
        allow-partial-reveals: allow-partial-reveals
      }
    )
    (ok true)
  )
)

;; Make anonymous donation commitment
(define-public (commit-anonymous-donation (commitment-hash (buff 32)) (campaign-id uint) (amount uint))
  (let 
    ((campaign-totals (get-campaign-anonymous-total campaign-id)))
    
    ;; Validate inputs
    (asserts! (>= amount min-donation-amount) err-insufficient-amount)
    (asserts! (> amount u0) err-zero-amount)
    (asserts! (is-none (get-anonymous-commitment commitment-hash)) err-commitment-exists)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Store commitment
    (map-set anonymous-commitments
      { commitment-hash: commitment-hash }
      {
        campaign-id: campaign-id,
        committed-amount: amount,
        commitment-block: stacks-block-height,
        is-revealed: false,
        revealed-donor: none,
        revealed-at: none
      }
    )
    
    ;; Update campaign totals
    (map-set campaign-anonymous-totals
      { campaign-id: campaign-id }
      {
        total-anonymous: (+ (get total-anonymous campaign-totals) amount),
        commitment-count: (+ (get commitment-count campaign-totals) u1)
      }
    )
    
    ;; Update global counter
    (var-set total-anonymous-donations (+ (var-get total-anonymous-donations) u1))
    
    (ok commitment-hash)
  )
)

;; Reveal anonymous donation (within reveal window)
(define-public (reveal-donation (donor principal) (amount uint) (campaign-id uint) (nonce uint) (reveal-message (string-ascii 200)))
  (let 
    ((commitment-hash (generate-commitment-hash donor amount campaign-id nonce))
     (commitment (unwrap! (get-anonymous-commitment commitment-hash) err-commitment-not-found)))
    
    ;; Validate reveal parameters
    (asserts! (is-eq donor tx-sender) err-invalid-reveal)
    (asserts! (is-eq amount (get committed-amount commitment)) err-invalid-reveal)
    (asserts! (is-eq campaign-id (get campaign-id commitment)) err-invalid-reveal)
    (asserts! (not (get is-revealed commitment)) err-already-revealed)
    (asserts! (is-reveal-window-open commitment-hash) err-reveal-window-closed)
    
    ;; Mark nonce as used
    (map-set commitment-nonces
      { donor: donor, nonce: nonce }
      { used: true }
    )
    
    ;; Update commitment as revealed
    (map-set anonymous-commitments
      { commitment-hash: commitment-hash }
      (merge commitment {
        is-revealed: true,
        revealed-donor: (some donor),
        revealed-at: (some stacks-block-height)
      })
    )
    
    ;; Store reveal details
    (map-set revealed-donations
      { commitment-hash: commitment-hash }
      {
        donor: donor,
        campaign-id: campaign-id,
        amount: amount,
        reveal-message: reveal-message,
        revealed-at: stacks-block-height
      }
    )
    
    (ok true)
  )
)
