;; Harvest Monitoring System
;; Track, validate, and manage crop harvests through transparent blockchain ledger

;; ===============================================
;; DATA STORAGE
;; ===============================================

;; Master counter for harvest entries
(define-data-var harvest-counter uint u0)

;; Core harvest database
(define-map harvest-registry
  { harvest-id: uint }
  {
    crop-name: (string-ascii 64),
    farmer: principal,
    yield-quantity: uint,
    registration-block: uint,
    field-description: (string-ascii 128),
    classification-tags: (list 10 (string-ascii 32))
  }
)

;; Permissions registry for harvest data access
(define-map viewing-permissions
  { harvest-id: uint, viewer: principal }
  { access-allowed: bool }
)

;; ===============================================
;; UTILITY FUNCTIONS
;; ===============================================

;; Verify harvest record existence
(define-private (harvest-registered (harvest-id uint))
  (is-some (map-get? harvest-registry { harvest-id: harvest-id }))
)

;; Verify tag format compliance
(define-private (valid-tag-format (tag (string-ascii 32)))
  (and
    (> (len tag) u0)
    (< (len tag) u33)
  )
)

;; Validate the entire set of classification tags
(define-private (verify-tag-collection (tags (list 10 (string-ascii 32))))
  (and
    (> (len tags) u0)
    (<= (len tags) u10)
    (is-eq (len (filter valid-tag-format tags)) (len tags))
  )
)

;; Check if principal is the harvest owner
(define-private (verify-ownership (harvest-id uint) (farmer principal))
  (match (map-get? harvest-registry { harvest-id: harvest-id })
    harvest-details (is-eq (get farmer harvest-details) farmer)
    false
  )
)

;; Retrieve yield quantity for a harvest
(define-private (get-yield-amount (harvest-id uint))
  (default-to u0
    (get yield-quantity
      (map-get? harvest-registry { harvest-id: harvest-id })
    )
  )
)

;; ===============================================
;; SYSTEM CONSTANTS
;; ===============================================

;; Administrative control
(define-constant admin-key tx-sender)

;; Response Status Codes
(define-constant harvest-nonexistent (err u301))
(define-constant harvest-duplicate (err u302))
(define-constant title-constraint (err u303))
(define-constant volume-constraint (err u304))
(define-constant unauthorized-access (err u305))
(define-constant invalid-ownership (err u306))
(define-constant admin-only (err u300))
(define-constant viewing-restricted (err u307))
(define-constant tag-validation (err u308))

;; ===============================================
;; HARVEST MANAGEMENT FUNCTIONS
;; ===============================================

