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

;; Register new harvest with comprehensive details
(define-public (register-harvest 
  (crop (string-ascii 64)) 
  (quantity uint) 
  (description (string-ascii 128)) 
  (tags (list 10 (string-ascii 32)))
)
  (let
    (
      (new-harvest-id (+ (var-get harvest-counter) u1))
    )
    ;; Input validation
    (asserts! (> (len crop) u0) title-constraint)
    (asserts! (< (len crop) u65) title-constraint)
    (asserts! (> quantity u0) volume-constraint)
    (asserts! (< quantity u1000000000) volume-constraint)
    (asserts! (> (len description) u0) title-constraint)
    (asserts! (< (len description) u129) title-constraint)
    (asserts! (verify-tag-collection tags) tag-validation)

    ;; Create harvest record
    (map-insert harvest-registry
      { harvest-id: new-harvest-id }
      {
        crop-name: crop,
        farmer: tx-sender,
        yield-quantity: quantity,
        registration-block: block-height,
        field-description: description,
        classification-tags: tags
      }
    )

    ;; Grant access to harvest owner
    (map-insert viewing-permissions
      { harvest-id: new-harvest-id, viewer: tx-sender }
      { access-allowed: true }
    )

    ;; Update harvest counter
    (var-set harvest-counter new-harvest-id)
    (ok new-harvest-id)
  )
)

;; ===============================================
;; VERIFICATION FUNCTIONS
;; ===============================================

;; Authenticate harvest legitimacy and ownership history
(define-public (authenticate-harvest (harvest-id uint) (assumed-farmer principal))
  (let
    (
      (harvest-details (unwrap! (map-get? harvest-registry { harvest-id: harvest-id }) harvest-nonexistent))
      (actual-farmer (get farmer harvest-details))
      (planting-block (get registration-block harvest-details))
      (access-permitted (default-to 
        false 
        (get access-allowed 
          (map-get? viewing-permissions { harvest-id: harvest-id, viewer: tx-sender })
        )
      ))
    )
    ;; Verify access permissions
    (asserts! (harvest-registered harvest-id) harvest-nonexistent)
    (asserts! 
      (or 
        (is-eq tx-sender actual-farmer)
        access-permitted
        (is-eq tx-sender admin-key)
      ) 
      unauthorized-access
    )

    ;; Compare expected vs actual ownership
    (if (is-eq actual-farmer assumed-farmer)
      ;; Return authentication success with metadata
      (ok {
        is-authentic: true,
        current-block: block-height,
        blockchain-age: (- block-height planting-block),
        farmer-match: true
      })
      ;; Return authentication failure
      (ok {
        is-authentic: false,
        current-block: block-height,
        blockchain-age: (- block-height planting-block),
        farmer-match: false
      })
    )
  )
)

;; ===============================================
;; ACCESS CONTROL FUNCTIONS
;; ===============================================

;; Transfer harvest ownership to new farmer
(define-public (transfer-harvest (harvest-id uint) (new-farmer principal))
  (let
    (
      (harvest-details (unwrap! (map-get? harvest-registry { harvest-id: harvest-id }) harvest-nonexistent))
    )
    ;; Verify caller is the current owner
    (asserts! (harvest-registered harvest-id) harvest-nonexistent)
    (asserts! (is-eq (get farmer harvest-details) tx-sender) invalid-ownership)

    ;; Update ownership
    (map-set harvest-registry
      { harvest-id: harvest-id }
      (merge harvest-details { farmer: new-farmer })
    )
    (ok true)
  )
)

;; Remove viewing access for specific principal
(define-public (restrict-access (harvest-id uint) (viewer principal))
  (let
    (
      (harvest-details (unwrap! (map-get? harvest-registry { harvest-id: harvest-id }) harvest-nonexistent))
    )
    ;; Verify harvest exists and caller is the owner
    (asserts! (harvest-registered harvest-id) harvest-nonexistent)
    (asserts! (is-eq (get farmer harvest-details) tx-sender) invalid-ownership)
    (asserts! (not (is-eq viewer tx-sender)) admin-only)

    ;; Remove viewing permission
    (map-delete viewing-permissions { harvest-id: harvest-id, viewer: viewer })
    (ok true)
  )
)


;; ===============================================
;; MODIFICATION FUNCTIONS
;; ===============================================

;; Expand harvest classification with additional tags
(define-public (add-classification-tags (harvest-id uint) (additional-tags (list 10 (string-ascii 32))))
  (let
    (
      (harvest-details (unwrap! (map-get? harvest-registry { harvest-id: harvest-id }) harvest-nonexistent))
      (existing-tags (get classification-tags harvest-details))
      (combined-tags (unwrap! (as-max-len? (concat existing-tags additional-tags) u10) tag-validation))
    )
    ;; Verify harvest exists and caller is the owner
    (asserts! (harvest-registered harvest-id) harvest-nonexistent)
    (asserts! (is-eq (get farmer harvest-details) tx-sender) invalid-ownership)

    ;; Validate new tags format
    (asserts! (verify-tag-collection additional-tags) tag-validation)

    ;; Update harvest with combined tags
    (map-set harvest-registry
      { harvest-id: harvest-id }
      (merge harvest-details { classification-tags: combined-tags })
    )
    (ok combined-tags)
  )
)

;; Update harvest record with new information
(define-public (update-harvest 
  (harvest-id uint) 
  (new-crop-name (string-ascii 64)) 
  (new-quantity uint) 
  (new-description (string-ascii 128)) 
  (new-tags (list 10 (string-ascii 32)))
)
  (let
    (
      (harvest-details (unwrap! (map-get? harvest-registry { harvest-id: harvest-id }) harvest-nonexistent))
    )
    ;; Validate ownership and input
    (asserts! (harvest-registered harvest-id) harvest-nonexistent)
    (asserts! (is-eq (get farmer harvest-details) tx-sender) invalid-ownership)
    (asserts! (> (len new-crop-name) u0) title-constraint)
    (asserts! (< (len new-crop-name) u65) title-constraint)
    (asserts! (> new-quantity u0) volume-constraint)
    (asserts! (< new-quantity u1000000000) volume-constraint)
    (asserts! (> (len new-description) u0) title-constraint)
    (asserts! (< (len new-description) u129) title-constraint)
    (asserts! (verify-tag-collection new-tags) tag-validation)

    ;; Update harvest with new information
    (map-set harvest-registry
      { harvest-id: harvest-id }
      (merge harvest-details { 
        crop-name: new-crop-name, 
        yield-quantity: new-quantity, 
        field-description: new-description, 
        classification-tags: new-tags 
      })
    )
    (ok true)
  )
)

;; Delete harvest record from system
(define-public (remove-harvest (harvest-id uint))
  (let
    (
      (harvest-details (unwrap! (map-get? harvest-registry { harvest-id: harvest-id }) harvest-nonexistent))
    )
    ;; Ownership verification
    (asserts! (harvest-registered harvest-id) harvest-nonexistent)
    (asserts! (is-eq (get farmer harvest-details) tx-sender) invalid-ownership)

    ;; Remove harvest record
    (map-delete harvest-registry { harvest-id: harvest-id })
    (ok true)
  )
)

;; ===============================================
;; SECURITY FUNCTIONS
;; ===============================================

;; Apply emergency restriction to harvest record
(define-public (emergency-lock-harvest (harvest-id uint))
  (let
    (
      (harvest-details (unwrap! (map-get? harvest-registry { harvest-id: harvest-id }) harvest-nonexistent))
      (security-tag "SECURITY-LOCK")
      (existing-tags (get classification-tags harvest-details))
    )
    ;; Verify caller is either the owner or system admin
    (asserts! (harvest-registered harvest-id) harvest-nonexistent)
    (asserts! 
      (or 
        (is-eq tx-sender admin-key)
        (is-eq (get farmer harvest-details) tx-sender)
      ) 
      admin-only
    )

    (ok true)
  )
)

