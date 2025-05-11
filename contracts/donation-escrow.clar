
;; title: donation-escrow


(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-goal-not-met (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-already-funded (err u105))
(define-constant err-already-verified (err u106))
(define-constant err-already-claimed (err u107))
(define-constant err-deadline-passed (err u108))
(define-constant err-deadline-not-passed (err u109))
(define-constant err-zero-amount (err u110))

(define-data-var next-campaign-id uint u1)

(define-map campaigns
  { campaign-id: uint }
  {
    owner: principal,
    name: (string-ascii 100),
    description: (string-ascii 500),
    goal-amount: uint,
    current-amount: uint,
    deadline: uint,
    is-verified: bool,
    is-completed: bool,
    is-funds-released: bool
  }
)

(define-map donations
  { campaign-id: uint, donor: principal }
  { amount: uint, claimed: bool }
)

(define-map verifiers
  { verifier: principal }
  { active: bool }
)

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)

(define-read-only (get-donation (campaign-id uint) (donor principal))
  (map-get? donations { campaign-id: campaign-id, donor: donor })
)

(define-read-only (is-verifier (address principal))
  (default-to false (get active (map-get? verifiers { verifier: address })))
)

(define-read-only (get-next-campaign-id)
  (var-get next-campaign-id)
)

(define-read-only (is-owner)
  (is-eq tx-sender contract-owner)
)

(define-read-only (goal-reached (campaign-id uint))
  (match (get-campaign campaign-id)
    campaign (>= (get current-amount campaign) (get goal-amount campaign))
    false
  )
)

(define-read-only (deadline-passed (campaign-id uint))
  (match (get-campaign campaign-id)
    campaign (> stacks-block-height (get deadline campaign))
    false
  )
)

(define-public (create-campaign (name (string-ascii 100)) (description (string-ascii 500)) (goal-amount uint) (deadline uint))
  (let
    ((campaign-id (var-get next-campaign-id)))
    (asserts! (> goal-amount u0) err-zero-amount)
    (asserts! (> deadline stacks-block-height) err-deadline-passed)
    (map-insert campaigns
      { campaign-id: campaign-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        goal-amount: goal-amount,
        current-amount: u0,
        deadline: deadline,
        is-verified: false,
        is-completed: false,
        is-funds-released: false
      }
    )
    (var-set next-campaign-id (+ campaign-id u1))
    (ok campaign-id)
  )
)

(define-public (donate (campaign-id uint) (amount uint))
  (let
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found))
     (current-donation (get-donation campaign-id tx-sender)))
    (asserts! (> amount u0) err-zero-amount)
    (asserts! (not (get is-completed campaign)) err-already-funded)
    (asserts! (< stacks-block-height (get deadline campaign)) err-deadline-passed)
    
    (match current-donation
      existing-donation (begin
        (map-set donations
          { campaign-id: campaign-id, donor: tx-sender }
          { amount: (+ amount (get amount existing-donation)), claimed: false }
        )
      )
      (map-insert donations
        { campaign-id: campaign-id, donor: tx-sender }
        { amount: amount, claimed: false }
      )
    )
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { current-amount: (+ (get current-amount campaign) amount) })
    )
    
    (stx-transfer? amount tx-sender (as-contract tx-sender))
  )
)

(define-public (verify-campaign (campaign-id uint))
  (let
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found)))
    (asserts! (is-verifier tx-sender) err-unauthorized)
    (asserts! (not (get is-verified campaign)) err-already-verified)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { is-verified: true })
    )
    (ok true)
  )
)

(define-public (complete-campaign (campaign-id uint))
  (let
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-verified campaign) err-unauthorized)
    (asserts! (goal-reached campaign-id) err-goal-not-met)
    (asserts! (not (get is-completed campaign)) err-already-funded)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { is-completed: true })
    )
    (ok true)
  )
)

(define-public (release-funds (campaign-id uint))
  (let
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-completed campaign) err-unauthorized)
    (asserts! (not (get is-funds-released campaign)) err-already-claimed)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { is-funds-released: true })
    )
    
    (as-contract (stx-transfer? (get current-amount campaign) tx-sender (get owner campaign)))
  )
)

(define-public (claim-refund (campaign-id uint))
  (let
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found))
     (donation (unwrap! (get-donation campaign-id tx-sender) err-not-found)))
    (asserts! (deadline-passed campaign-id) err-deadline-not-passed)
    (asserts! (not (goal-reached campaign-id)) err-goal-not-met)
    (asserts! (not (get claimed donation)) err-already-claimed)
    
    (map-set donations
      { campaign-id: campaign-id, donor: tx-sender }
      (merge donation { claimed: true })
    )
    
    (as-contract (stx-transfer? (get amount donation) tx-sender tx-sender))
  )
)

(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set verifiers
      { verifier: verifier }
      { active: true }
    )
    (ok true)
  )
)

(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete verifiers { verifier: verifier })
    (ok true)
  )
)