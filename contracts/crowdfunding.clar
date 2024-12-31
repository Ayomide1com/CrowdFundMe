(define-map campaigns
  { id: uint }
  { creator: principal, title: (string-ascii 100), description: (string-ascii 500), goal: uint, deadline: uint, raised: uint, withdrawn: bool })

(define-map contributions
  { campaign-id: uint, contributor: principal }
  { amount: uint })

(define-data-var campaign-counter uint u0)

(define-public (create-campaign (title (string-ascii 100)) (description (string-ascii 500)) (goal uint) (deadline uint))
  (let ((id (var-get campaign-counter)))
    (begin
      (map-set campaigns
        { id: id }
        { creator: tx-sender, title: title, description: description, goal: goal, deadline: deadline, raised: u0, withdrawn: false })
      (var-set campaign-counter (+ id u1))
      (ok id))))

(define-public (contribute (campaign-id uint))
  (let ((campaign (map-get? campaigns { id: campaign-id }))
        (amount (stx-get-transfer-amount)))
    (if (and campaign (> amount u0))
      (let ((new-raised (+ (get raised campaign) amount)))
        (begin
          (map-set campaigns { id: campaign-id } (merge campaign { raised: new-raised }))
          (map-set contributions { campaign-id: campaign-id, contributor: tx-sender } { amount: amount })
          (ok "Contribution successful")))
      (err "Invalid campaign or amount"))))

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { id: campaign-id }))

(define-public (withdraw-funds (campaign-id uint))
  (let ((campaign (map-get? campaigns { id: campaign-id })))
    (if (and campaign (is-eq (get creator campaign) tx-sender) (>= (get raised campaign) (get goal campaign)) (not (get withdrawn campaign)))
      (begin
        (map-set campaigns { id: campaign-id } (merge campaign { withdrawn: true }))
        (stx-transfer? (get raised campaign) tx-sender)
        (ok "Funds withdrawn"))
      (err "Unauthorized or invalid conditions for withdrawal"))))

(define-public (refund (campaign-id uint))
  (let ((campaign (map-get? campaigns { id: campaign-id }))
        (contribution (map-get? contributions { campaign-id: campaign-id, contributor: tx-sender })))
    (if (and campaign contribution (< (get raised campaign) (get goal campaign)) (> (get deadline campaign) block-height))
      (begin
        (map-delete contributions { campaign-id: campaign-id, contributor: tx-sender })
        (stx-transfer? (get amount contribution) tx-sender)
        (ok "Refund issued"))
      (err "Refund not applicable"))))
