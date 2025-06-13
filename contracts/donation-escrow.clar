
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



(define-map campaign-categories 
  { category-id: uint }
  { name: (string-ascii 50) }
)

(define-map campaign-category-mapping
  { campaign-id: uint }
  { category-id: uint }
)

(define-public (add-category (category-id uint) (name (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set campaign-categories
      { category-id: category-id }
      { name: name }
    )
    (ok true)
  )
)

(define-public (set-campaign-category (campaign-id uint) (category-id uint))
  (let ((campaign (unwrap! (get-campaign campaign-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (map-set campaign-category-mapping
      { campaign-id: campaign-id }
      { category-id: category-id }
    )
    (ok true)
  )
)

(define-read-only (get-campaigns-by-category (category-id uint))
  (ok (map-get? campaigns { campaign-id: category-id }))
)

(define-map campaign-updates
  { campaign-id: uint, update-id: uint }
  {
    title: (string-ascii 100),
    content: (string-ascii 500),
    block-height: uint
  }
)

(define-map campaign-milestones
  { campaign-id: uint, milestone-id: uint }
  {
    title: (string-ascii 100),
    target-amount: uint,
    is-reached: bool
  }
)

(define-data-var next-update-id uint u1)
(define-data-var next-milestone-id uint u1)

(define-public (post-update (campaign-id uint) (title (string-ascii 100)) (content (string-ascii 500)))
  (let 
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found))
     (update-id (var-get next-update-id)))
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (map-set campaign-updates
      { campaign-id: campaign-id, update-id: update-id }
      { 
        title: title,
        content: content,
        block-height: stacks-block-height 
      }
    )
    (var-set next-update-id (+ update-id u1))
    (ok update-id)
  )
)

(define-public (add-milestone (campaign-id uint) (title (string-ascii 100)) (target-amount uint))
  (let 
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found))
     (milestone-id (var-get next-milestone-id)))
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (map-set campaign-milestones
      { campaign-id: campaign-id, milestone-id: milestone-id }
      {
        title: title,
        target-amount: target-amount,
        is-reached: false
      }
    )
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)


(define-map matching-pledges
  { campaign-id: uint, sponsor: principal }
  {
    match-ratio: uint,
    max-match-amount: uint,
    current-matched: uint,
    is-active: bool
  }
)

(define-map matched-donations
  { campaign-id: uint, donor: principal, sponsor: principal }
  { matched-amount: uint }
)

(define-constant err-invalid-ratio (err u111))
(define-constant err-match-exceeded (err u112))
(define-constant err-match-inactive (err u113))

(define-public (create-matching-pledge (campaign-id uint) (match-ratio uint) (max-match-amount uint))
  (let ((campaign (unwrap! (get-campaign campaign-id) err-not-found)))
    (asserts! (> match-ratio u0) err-invalid-ratio)
    (asserts! (<= match-ratio u100) err-invalid-ratio)
    (asserts! (> max-match-amount u0) err-zero-amount)
    (asserts! (< stacks-block-height (get deadline campaign)) err-deadline-passed)
    
    (try! (stx-transfer? max-match-amount tx-sender (as-contract tx-sender)))
    
    (map-set matching-pledges
      { campaign-id: campaign-id, sponsor: tx-sender }
      {
        match-ratio: match-ratio,
        max-match-amount: max-match-amount,
        current-matched: u0,
        is-active: true
      }
    )
    (ok true)
  )
)

(define-public (donate-with-matching (campaign-id uint) (amount uint) (sponsor principal))
  (let 
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found))
     (pledge (unwrap! (map-get? matching-pledges { campaign-id: campaign-id, sponsor: sponsor }) err-not-found))
     (match-amount (/ (* amount (get match-ratio pledge)) u100))
     (available-match (- (get max-match-amount pledge) (get current-matched pledge)))
     (actual-match (if (<= match-amount available-match) match-amount available-match))
     (current-donation (get-donation campaign-id tx-sender)))
    
    (asserts! (> amount u0) err-zero-amount)
    (asserts! (not (get is-completed campaign)) err-already-funded)
    (asserts! (< stacks-block-height (get deadline campaign)) err-deadline-passed)
    (asserts! (get is-active pledge) err-match-inactive)
    (asserts! (> actual-match u0) err-match-exceeded)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
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
    
    (map-set matched-donations
      { campaign-id: campaign-id, donor: tx-sender, sponsor: sponsor }
      { matched-amount: actual-match }
    )
    
    (map-set matching-pledges
      { campaign-id: campaign-id, sponsor: sponsor }
      (merge pledge { current-matched: (+ (get current-matched pledge) actual-match) })
    )
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { current-amount: (+ (get current-amount campaign) amount actual-match) })
    )
    
    (ok actual-match)
  )
)

(define-public (deactivate-matching-pledge (campaign-id uint))
  (let ((pledge (unwrap! (map-get? matching-pledges { campaign-id: campaign-id, sponsor: tx-sender }) err-not-found)))
    (asserts! (get is-active pledge) err-match-inactive)
    
    (map-set matching-pledges
      { campaign-id: campaign-id, sponsor: tx-sender }
      (merge pledge { is-active: false })
    )
    
    (let ((unused-amount (- (get max-match-amount pledge) (get current-matched pledge))))
      (if (> unused-amount u0)
        (as-contract (stx-transfer? unused-amount tx-sender tx-sender))
        (ok true)
      )
    )
  )
)

(define-read-only (get-matching-pledge (campaign-id uint) (sponsor principal))
  (map-get? matching-pledges { campaign-id: campaign-id, sponsor: sponsor })
)

(define-read-only (get-matched-donation (campaign-id uint) (donor principal) (sponsor principal))
  (map-get? matched-donations { campaign-id: campaign-id, donor: donor, sponsor: sponsor })
)

(define-read-only (calculate-match-amount (campaign-id uint) (sponsor principal) (donation-amount uint))
  (match (get-matching-pledge campaign-id sponsor)
    pledge 
      (let 
        ((match-amount (/ (* donation-amount (get match-ratio pledge)) u100))
         (available-match (- (get max-match-amount pledge) (get current-matched pledge))))
        (ok (if (<= match-amount available-match) match-amount available-match))
      )
    (ok u0)
  )
)