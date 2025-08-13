
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
    
    (begin
      (update-donor-stats tx-sender amount campaign-id)
      (stx-transfer? amount tx-sender (as-contract tx-sender))
    )
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
    
    (begin
      (update-donor-stats tx-sender amount campaign-id)
      (ok actual-match)
    )
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

(define-map campaign-tips
  { campaign-id: uint, tipper: principal }
  { total-tips: uint }
)

(define-map campaign-tip-totals
  { campaign-id: uint }
  { total-tipped: uint }
)

(define-constant err-tip-too-small (err u114))
(define-constant minimum-tip-amount u1000)

(define-public (tip-campaign (campaign-id uint) (tip-amount uint))
  (let 
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found))
     (current-tips (default-to { total-tips: u0 } (map-get? campaign-tips { campaign-id: campaign-id, tipper: tx-sender })))
     (current-total (default-to { total-tipped: u0 } (map-get? campaign-tip-totals { campaign-id: campaign-id }))))
    (asserts! (>= tip-amount minimum-tip-amount) err-tip-too-small)
    (asserts! (not (is-eq tx-sender (get owner campaign))) err-unauthorized)
    
    (try! (stx-transfer? tip-amount tx-sender (get owner campaign)))
    
    (map-set campaign-tips
      { campaign-id: campaign-id, tipper: tx-sender }
      { total-tips: (+ (get total-tips current-tips) tip-amount) }
    )
    
    (map-set campaign-tip-totals
      { campaign-id: campaign-id }
      { total-tipped: (+ (get total-tipped current-total) tip-amount) }
    )
    
    (ok tip-amount)
  )
)

(define-read-only (get-campaign-tips (campaign-id uint) (tipper principal))
  (map-get? campaign-tips { campaign-id: campaign-id, tipper: tipper })
)

(define-read-only (get-campaign-tip-total (campaign-id uint))
  (default-to u0 (get total-tipped (map-get? campaign-tip-totals { campaign-id: campaign-id })))
)

(define-read-only (get-top-tippers (campaign-id uint))
  (ok (get-campaign-tip-total campaign-id))
)

(define-map global-donor-stats
  { donor: principal }
  {
    total-donated: uint,
    campaigns-supported: uint,
    largest-single-donation: uint,
    rank-points: uint
  }
)

(define-map leaderboard-rankings
  { rank: uint }
  { donor: principal, total-donated: uint }
)

(define-data-var total-leaderboard-entries uint u0)
(define-constant max-leaderboard-size u50)

(define-private (update-donor-stats (donor principal) (donation-amount uint) (campaign-id uint))
  (let 
    ((current-stats (default-to 
      { total-donated: u0, campaigns-supported: u0, largest-single-donation: u0, rank-points: u0 } 
      (map-get? global-donor-stats { donor: donor })))
     (is-new-campaign (is-none (get-donation campaign-id donor)))
     (new-total (+ (get total-donated current-stats) donation-amount))
     (new-campaigns (if is-new-campaign 
       (+ (get campaigns-supported current-stats) u1) 
       (get campaigns-supported current-stats)))
     (new-largest (if (> donation-amount (get largest-single-donation current-stats)) 
       donation-amount 
       (get largest-single-donation current-stats)))
     (new-rank-points (+ (* new-total u1) (* new-campaigns u1000) (* new-largest u10))))
    
    (map-set global-donor-stats
      { donor: donor }
      {
        total-donated: new-total,
        campaigns-supported: new-campaigns,
        largest-single-donation: new-largest,
        rank-points: new-rank-points
      }
    )
    (update-leaderboard donor new-total)
    true
  )
)

(define-private (update-leaderboard (donor principal) (total-donated uint))
  (let ((current-entries (var-get total-leaderboard-entries)))
    (if (< current-entries max-leaderboard-size)
      (begin
        (map-set leaderboard-rankings
          { rank: (+ current-entries u1) }
          { donor: donor, total-donated: total-donated }
        )
        (var-set total-leaderboard-entries (+ current-entries u1))
      )
      (insert-into-sorted-leaderboard donor total-donated)
    )
  )
)

(define-private (insert-into-sorted-leaderboard (donor principal) (total-donated uint))
  (let ((lowest-entry (map-get? leaderboard-rankings { rank: max-leaderboard-size })))
    (match lowest-entry
      entry 
        (if (> total-donated (get total-donated entry))
          (map-set leaderboard-rankings
            { rank: max-leaderboard-size }
            { donor: donor, total-donated: total-donated }
          )
          true
        )
      true
    )
  )
)

(define-read-only (get-donor-stats (donor principal))
  (map-get? global-donor-stats { donor: donor })
)

(define-read-only (get-leaderboard-entry (rank uint))
  (map-get? leaderboard-rankings { rank: rank })
)

(define-read-only (get-donor-rank (donor principal))
  (let ((donor-stats (map-get? global-donor-stats { donor: donor })))
    (match donor-stats
      stats 
        (let ((total-donated (get total-donated stats)))
          (get found-rank (fold check-rank-position 
            (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50)
            { donor: donor, target-donated: total-donated, found-rank: (some u999) }))
        )
      (some u999)
    )
  )
)

(define-private (check-rank-position (rank uint) (state { donor: principal, target-donated: uint, found-rank: (optional uint) }))
  (if (is-some (get found-rank state))
    state
    (match (get-leaderboard-entry rank)
      entry 
        (if (is-eq (get donor entry) (get donor state))
          (merge state { found-rank: (some rank) })
          state
        )
      state
    )
  )
)

(define-read-only (get-top-donors (limit uint))
  (let ((actual-limit (if (> limit max-leaderboard-size) max-leaderboard-size limit)))
    (map get-leaderboard-entry 
      (unwrap! (slice? 
        (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50) 
        u0 
        actual-limit) 
      (list))
    )
  )
)

;; Phased Fund Withdrawal System
;; Allows controlled release of campaign funds in predetermined phases

(define-map withdrawal-phases
  { campaign-id: uint, phase-id: uint }
  {
    percentage: uint,        ;; Percentage of total funds for this phase (0-100)
    description: (string-ascii 200),
    is-approved: bool,
    is-withdrawn: bool,
    withdrawn-amount: uint,
    approved-by: (optional principal),
    approved-at: (optional uint)
  }
)

(define-map campaign-withdrawal-config
  { campaign-id: uint }
  {
    total-phases: uint,
    phases-created: uint,
    total-withdrawn: uint,
    is-phased-enabled: bool,
    requires-verification: bool
  }
)

(define-map phase-withdrawal-requests
  { campaign-id: uint, phase-id: uint }
  {
    requested-by: principal,
    requested-at: uint,
    justification: (string-ascii 300),
    status: (string-ascii 20)  ;; "pending", "approved", "rejected"
  }
)

;; Error constants for phased withdrawals
(define-constant err-phase-not-found (err u115))
(define-constant err-phase-already-exists (err u116))
(define-constant err-phase-not-approved (err u117))
(define-constant err-phase-already-withdrawn (err u118))
(define-constant err-invalid-percentage (err u119))
(define-constant err-phases-exceed-100 (err u120))
(define-constant err-phased-not-enabled (err u121))
(define-constant err-request-already-exists (err u122))
(define-constant err-invalid-phase-order (err u123))

;; Enable phased withdrawal for a campaign (only owner can call)
(define-public (enable-phased-withdrawal (campaign-id uint) (total-phases uint) (requires-verification bool))
  (let ((campaign (unwrap! (get-campaign campaign-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (not (get is-completed campaign)) err-already-funded)
    (asserts! (> total-phases u0) err-zero-amount)
    (asserts! (<= total-phases u10) err-invalid-percentage) ;; Max 10 phases
    
    (map-set campaign-withdrawal-config
      { campaign-id: campaign-id }
      {
        total-phases: total-phases,
        phases-created: u0,
        total-withdrawn: u0,
        is-phased-enabled: true,
        requires-verification: requires-verification
      }
    )
    (ok true)
  )
)

;; Create a withdrawal phase (only campaign owner)
(define-public (create-withdrawal-phase (campaign-id uint) (phase-id uint) (percentage uint) (description (string-ascii 200)))
  (let 
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found))
     (config (unwrap! (map-get? campaign-withdrawal-config { campaign-id: campaign-id }) err-phased-not-enabled)))
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-phased-enabled config) err-phased-not-enabled)
    (asserts! (> percentage u0) err-invalid-percentage)
    (asserts! (<= percentage u100) err-invalid-percentage)
    (asserts! (< (get phases-created config) (get total-phases config)) err-phase-already-exists)
    
    ;; Check if phase already exists
    (asserts! (is-none (map-get? withdrawal-phases { campaign-id: campaign-id, phase-id: phase-id })) err-phase-already-exists)
    
    ;; Validate total percentage doesn't exceed 100%
    (let ((total-percentage (+ percentage (get-total-phase-percentage campaign-id))))
      (asserts! (<= total-percentage u100) err-phases-exceed-100)
    )
    
    ;; Create the phase
    (map-set withdrawal-phases
      { campaign-id: campaign-id, phase-id: phase-id }
      {
        percentage: percentage,
        description: description,
        is-approved: false,
        is-withdrawn: false,
        withdrawn-amount: u0,
        approved-by: none,
        approved-at: none
      }
    )
    
    ;; Update config
    (map-set campaign-withdrawal-config
      { campaign-id: campaign-id }
      (merge config { phases-created: (+ (get phases-created config) u1) })
    )
    
    (ok phase-id)
  )
)

;; Request phase withdrawal (only campaign owner)
(define-public (request-phase-withdrawal (campaign-id uint) (phase-id uint) (justification (string-ascii 300)))
  (let 
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found))
     (phase (unwrap! (map-get? withdrawal-phases { campaign-id: campaign-id, phase-id: phase-id }) err-phase-not-found))
     (config (unwrap! (map-get? campaign-withdrawal-config { campaign-id: campaign-id }) err-phased-not-enabled)))
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-completed campaign) err-unauthorized)
    (asserts! (not (get is-withdrawn phase)) err-phase-already-withdrawn)
    
    ;; Check if request already exists
    (asserts! (is-none (map-get? phase-withdrawal-requests { campaign-id: campaign-id, phase-id: phase-id })) err-request-already-exists)
    
    ;; If verification not required, auto-approve
    (if (not (get requires-verification config))
      (begin
        (map-set withdrawal-phases
          { campaign-id: campaign-id, phase-id: phase-id }
          (merge phase { 
            is-approved: true, 
            approved-by: (some tx-sender),
            approved-at: (some stacks-block-height)
          })
        )
        (map-set phase-withdrawal-requests
          { campaign-id: campaign-id, phase-id: phase-id }
          {
            requested-by: tx-sender,
            requested-at: stacks-block-height,
            justification: justification,
            status: "approved"
          }
        )
      )
      (map-set phase-withdrawal-requests
        { campaign-id: campaign-id, phase-id: phase-id }
        {
          requested-by: tx-sender,
          requested-at: stacks-block-height,
          justification: justification,
          status: "pending"
        }
      )
    )
    
    (ok true)
  )
)

;; Approve phase withdrawal (only verifiers)
(define-public (approve-phase-withdrawal (campaign-id uint) (phase-id uint))
  (let 
    ((phase (unwrap! (map-get? withdrawal-phases { campaign-id: campaign-id, phase-id: phase-id }) err-phase-not-found))
     (request (unwrap! (map-get? phase-withdrawal-requests { campaign-id: campaign-id, phase-id: phase-id }) err-not-found)))
    
    (asserts! (is-verifier tx-sender) err-unauthorized)
    (asserts! (is-eq (get status request) "pending") err-phase-already-withdrawn)
    (asserts! (not (get is-approved phase)) err-phase-already-withdrawn)
    
    ;; Update phase approval
    (map-set withdrawal-phases
      { campaign-id: campaign-id, phase-id: phase-id }
      (merge phase { 
        is-approved: true,
        approved-by: (some tx-sender),
        approved-at: (some stacks-block-height)
      })
    )
    
    ;; Update request status
    (map-set phase-withdrawal-requests
      { campaign-id: campaign-id, phase-id: phase-id }
      (merge request { status: "approved" })
    )
    
    (ok true)
  )
)

;; Execute phase withdrawal (only campaign owner)
(define-public (execute-phase-withdrawal (campaign-id uint) (phase-id uint))
  (let 
    ((campaign (unwrap! (get-campaign campaign-id) err-not-found))
     (phase (unwrap! (map-get? withdrawal-phases { campaign-id: campaign-id, phase-id: phase-id }) err-phase-not-found))
     (config (unwrap! (map-get? campaign-withdrawal-config { campaign-id: campaign-id }) err-phased-not-enabled)))
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-approved phase) err-phase-not-approved)
    (asserts! (not (get is-withdrawn phase)) err-phase-already-withdrawn)
    
    ;; Calculate withdrawal amount
    (let 
      ((withdrawal-amount (/ (* (get current-amount campaign) (get percentage phase)) u100))
       (available-amount (- (get current-amount campaign) (get total-withdrawn config))))
      
      (asserts! (<= withdrawal-amount available-amount) err-goal-not-met)
      
      ;; Execute transfer
      (try! (as-contract (stx-transfer? withdrawal-amount tx-sender (get owner campaign))))
      
      ;; Update phase as withdrawn
      (map-set withdrawal-phases
        { campaign-id: campaign-id, phase-id: phase-id }
        (merge phase { 
          is-withdrawn: true,
          withdrawn-amount: withdrawal-amount
        })
      )
      
      ;; Update total withdrawn
      (map-set campaign-withdrawal-config
        { campaign-id: campaign-id }
        (merge config { total-withdrawn: (+ (get total-withdrawn config) withdrawal-amount) })
      )
      
      (ok withdrawal-amount)
    )
  )
)

;; Helper function to calculate total percentage of all phases
(define-private (get-total-phase-percentage (campaign-id uint))
  (let ((config (map-get? campaign-withdrawal-config { campaign-id: campaign-id })))
    (match config
      conf 
        (get total (fold calculate-phase-percentage 
          (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
          { campaign-id: campaign-id, total: u0 }))
      u0
    )
  )
)

(define-private (calculate-phase-percentage (phase-id uint) (state { campaign-id: uint, total: uint }))
  (let ((phase (map-get? withdrawal-phases { campaign-id: (get campaign-id state), phase-id: phase-id })))
    (match phase
      p (merge state { total: (+ (get total state) (get percentage p)) })
      state
    )
  )
)

;; Read-only functions
(define-read-only (get-withdrawal-phase (campaign-id uint) (phase-id uint))
  (map-get? withdrawal-phases { campaign-id: campaign-id, phase-id: phase-id })
)

(define-read-only (get-campaign-withdrawal-config (campaign-id uint))
  (map-get? campaign-withdrawal-config { campaign-id: campaign-id })
)

(define-read-only (get-phase-withdrawal-request (campaign-id uint) (phase-id uint))
  (map-get? phase-withdrawal-requests { campaign-id: campaign-id, phase-id: phase-id })
)

(define-read-only (is-phase-withdrawable (campaign-id uint) (phase-id uint))
  (match (get-withdrawal-phase campaign-id phase-id)
    phase (and (get is-approved phase) (not (get is-withdrawn phase)))
    false
  )
)

(define-read-only (get-remaining-withdrawal-amount (campaign-id uint))
  (match (get-campaign-withdrawal-config campaign-id)
    config 
      (match (get-campaign campaign-id)
        campaign (- (get current-amount campaign) (get total-withdrawn config))
        u0
      )
    u0
  )
)


