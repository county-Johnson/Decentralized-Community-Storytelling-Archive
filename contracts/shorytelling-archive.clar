;; ===============================================
;; DECENTRALIZED COMMUNITY STORYTELLING ARCHIVE
;; ===============================================
;; A platform for preserving local history and personal narratives
;; with multimedia documentation and intergenerational sharing

;; ===============================================
;; CONSTANTS AND ERROR CODES
;; ===============================================

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-insufficient-permissions (err u105))
(define-constant err-story-locked (err u106))
(define-constant err-invalid-status (err u107))
(define-constant err-curator-required (err u108))

;; Access control levels
(define-constant access-public u0)
(define-constant access-community u1)
(define-constant access-family u2)
(define-constant access-private u3)

;; Story status constants
(define-constant status-draft u0)
(define-constant status-published u1)
(define-constant status-archived u2)
(define-constant status-featured u3)

;; ===============================================
;; DATA VARIABLES
;; ===============================================

(define-data-var story-id-nonce uint u0)
(define-data-var collection-id-nonce uint u0)
(define-data-var platform-fee uint u100) ;; 1% in basis points
(define-data-var min-curator-stake uint u1000000) ;; 1 STX minimum

;; ===============================================
;; DATA MAPS
;; ===============================================

;; Core story data structure
(define-map stories
  uint
  {
    title: (string-ascii 200),
    storyteller: principal,
    narrator: principal,
    content-hash: (string-ascii 64), ;; IPFS hash for multimedia content
    metadata-hash: (string-ascii 64), ;; IPFS hash for metadata
    cultural-tags: (list 10 (string-ascii 50)),
    time-period: (string-ascii 100),
    location: (string-ascii 200),
    language: (string-ascii 20),
    access-level: uint,
    status: uint,
    created-at: uint,
    updated-at: uint,
    verification-score: uint,
    interaction-count: uint
  }
)

;; Story collections for thematic grouping
(define-map collections
  uint
  {
    name: (string-ascii 200),
    description: (string-ascii 500),
    creator: principal,
    cultural-context: (string-ascii 200),
    time-period: (string-ascii 100),
    access-level: uint,
    story-count: uint,
    created-at: uint,
    is-featured: bool
  }
)

;; Link stories to collections
(define-map collection-stories
  {collection-id: uint, story-id: uint}
  bool
)

;; Community curators with verification powers
(define-map curators
  principal
  {
    stake-amount: uint,
    verification-count: uint,
    reputation-score: uint,
    specialties: (list 5 (string-ascii 50)),
    registered-at: uint,
    is-active: bool
  }
)

;; Access control for stories
(define-map story-access
  {story-id: uint, user: principal}
  {
    permission-level: uint,
    granted-by: principal,
    granted-at: uint
  }
)

;; Cultural significance and community validation
(define-map cultural-significance
  uint
  {
    heritage-value: uint,
    community-endorsements: uint,
    elder-approvals: uint,
    cultural-accuracy: uint,
    preservation-priority: uint
  }
)

;; Intergenerational connections
(define-map story-connections
  {story-id: uint, connected-story-id: uint}
  {
    relationship-type: (string-ascii 50), ;; "continuation", "response", "related", "correction"
    description: (string-ascii 300),
    created-by: principal,
    created-at: uint
  }
)

;; Story interactions (likes, shares, comments)
(define-map story-interactions
  {story-id: uint, user: principal}
  {
    interaction-type: (string-ascii 20), ;; "like", "share", "comment", "save"
    timestamp: uint,
    comment-hash: (optional (string-ascii 64))
  }
)

;; Community verification votes
(define-map verification-votes
  {story-id: uint, curator: principal}
  {
    vote: bool,
    reasoning: (string-ascii 300),
    expertise-area: (string-ascii 50),
    timestamp: uint
  }
)

;; ===============================================
;; PRIVATE FUNCTIONS
;; ===============================================

(define-private (is-authorized (story-id uint) (user principal))
  (let ((story (unwrap! (map-get? stories story-id) false)))
    (or
      ;; Story owner always has access
      (is-eq user (get storyteller story))
      (is-eq user (get narrator story))
      ;; Public access
      (is-eq (get access-level story) access-public)
      ;; Check specific permissions
      (match (map-get? story-access {story-id: story-id, user: user})
        permission true
        false
      )
    )
  )
)

(define-private (validate-cultural-tags (tags (list 10 (string-ascii 50))))
  (< (len tags) u11)
)

(define-private (increment-story-id)
  (let ((current-id (var-get story-id-nonce)))
    (var-set story-id-nonce (+ current-id u1))
    current-id
  )
)

(define-private (increment-collection-id)
  (let ((current-id (var-get collection-id-nonce)))
    (var-set collection-id-nonce (+ current-id u1))
    current-id
  )
)

(define-private (calculate-verification-score (story-id uint))
  (let ((cultural-sig (default-to
                        {heritage-value: u0, community-endorsements: u0, elder-approvals: u0, cultural-accuracy: u0, preservation-priority: u0}
                        (map-get? cultural-significance story-id))))
    (+ (get heritage-value cultural-sig)
       (get community-endorsements cultural-sig)
       (get elder-approvals cultural-sig)
       (get cultural-accuracy cultural-sig)
       (get preservation-priority cultural-sig))
  )
)

;; ===============================================
;; PUBLIC FUNCTIONS - STORY MANAGEMENT
;; ===============================================

(define-public (create-story
  (title (string-ascii 200))
  (content-hash (string-ascii 64))
  (metadata-hash (string-ascii 64))
  (cultural-tags (list 10 (string-ascii 50)))
  (time-period (string-ascii 100))
  (location (string-ascii 200))
  (language (string-ascii 20))
  (access-level uint)
  (narrator principal))

  (let ((story-id (increment-story-id)))
    (asserts! (validate-cultural-tags cultural-tags) err-invalid-input)
    (asserts! (<= access-level access-private) err-invalid-input)
    (asserts! (> (len title) u0) err-invalid-input)
    (asserts! (> (len content-hash) u0) err-invalid-input)

    (try! (map-set stories story-id
      {
        title: title,
        storyteller: tx-sender,
        narrator: narrator,
        content-hash: content-hash,
        metadata-hash: metadata-hash,
        cultural-tags: cultural-tags,
        time-period: time-period,
        location: location,
        language: language,
        access-level: access-level,
        status: status-draft,
        created-at: stacks-block-height,
        updated-at: stacks-block-height,
        verification-score: u0,
        interaction-count: u0
      }
    ))

    ;; Initialize cultural significance tracking
    (map-set cultural-significance story-id
      {
        heritage-value: u0,
        community-endorsements: u0,
        elder-approvals: u0,
        cultural-accuracy: u0,
        preservation-priority: u0
      }
    )

    (ok story-id)
  )
)

(define-public (publish-story (story-id uint))
  (let ((story (unwrap! (map-get? stories story-id) err-not-found)))
    (asserts! (is-eq tx-sender (get storyteller story)) err-unauthorized)
    (asserts! (is-eq (get status story) status-draft) err-invalid-status)

    (try! (map-set stories story-id
      (merge story {
        status: status-published,
        updated-at: stacks-block-height
      })
    ))

    (ok true)
  )
)

(define-public (update-story-access
  (story-id uint)
  (user principal)
  (permission-level uint))

  (let ((story (unwrap! (map-get? stories story-id) err-not-found)))
    (asserts! (is-eq tx-sender (get storyteller story)) err-unauthorized)
    (asserts! (<= permission-level access-private) err-invalid-input)

    (try! (map-set story-access {story-id: story-id, user: user}
      {
        permission-level: permission-level,
        granted-by: tx-sender,
        granted-at: stacks-block-height
      }
    ))

    (ok true)
  )
)

;; ===============================================
;; PUBLIC FUNCTIONS - COLLECTION MANAGEMENT
;; ===============================================

(define-public (create-collection
  (name (string-ascii 200))
  (description (string-ascii 500))
  (cultural-context (string-ascii 200))
  (time-period (string-ascii 100))
  (access-level uint))

  (let ((collection-id (increment-collection-id)))
    (asserts! (> (len name) u0) err-invalid-input)
    (asserts! (<= access-level access-private) err-invalid-input)

    (try! (map-set collections collection-id
      {
        name: name,
        description: description,
        creator: tx-sender,
        cultural-context: cultural-context,
        time-period: time-period,
        access-level: access-level,
        story-count: u0,
        created-at: stacks-block-height,
        is-featured: false
      }
    ))

    (ok collection-id)
  )
)

(define-public (add-story-to-collection (collection-id uint) (story-id uint))
  (let ((collection (unwrap! (map-get? collections collection-id) err-not-found))
        (story (unwrap! (map-get? stories story-id) err-not-found)))

    (asserts! (is-eq tx-sender (get creator collection)) err-unauthorized)
    (asserts! (is-authorized story-id tx-sender) err-unauthorized)

    (try! (map-set collection-stories {collection-id: collection-id, story-id: story-id} true))

    (try! (map-set collections collection-id
      (merge collection {
        story-count: (+ (get story-count collection) u1)
      })
    ))

    (ok true)
  )
)

;; ===============================================
;; PUBLIC FUNCTIONS - CURATOR SYSTEM
;; ===============================================

(define-public (register-curator
  (stake-amount uint)
  (specialties (list 5 (string-ascii 50))))

  (asserts! (>= stake-amount (var-get min-curator-stake)) err-insufficient-permissions)
  (asserts! (is-none (map-get? curators tx-sender)) err-already-exists)

  ;; Transfer stake to contract
  (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))

  (try! (map-set curators tx-sender
    {
      stake-amount: stake-amount,
      verification-count: u0,
      reputation-score: u100, ;; Starting reputation
      specialties: specialties,
      registered-at: stacks-block-height,
      is-active: true
    }
  ))

  (ok true)
)

(define-public (verify-story (story-id uint) (vote bool) (reasoning (string-ascii 300)) (expertise-area (string-ascii 50)))
  (let ((story (unwrap! (map-get? stories story-id) err-not-found))
        (curator (unwrap! (map-get? curators tx-sender) err-curator-required)))

    (asserts! (get is-active curator) err-unauthorized)
    (asserts! (is-eq (get status story) status-published) err-invalid-status)

    (try! (map-set verification-votes {story-id: story-id, curator: tx-sender}
      {
        vote: vote,
        reasoning: reasoning,
        expertise-area: expertise-area,
        timestamp: stacks-block-height
      }
    ))

    ;; Update curator stats
    (try! (map-set curators tx-sender
      (merge curator {
        verification-count: (+ (get verification-count curator) u1),
        reputation-score: (if vote
                            (+ (get reputation-score curator) u5)
                            (get reputation-score curator))
      })
    ))

    ;; Update story verification score
    (let ((new-score (calculate-verification-score story-id)))
      (try! (map-set stories story-id
        (merge story {
          verification-score: new-score,
          updated-at: stacks-block-height
        })
      ))
    )

    (ok true)
  )
)

;; ===============================================
;; PUBLIC FUNCTIONS - COMMUNITY INTERACTION
;; ===============================================

(define-public (interact-with-story
  (story-id uint)
  (interaction-type (string-ascii 20))
  (comment-hash (optional (string-ascii 64))))

  (let ((story (unwrap! (map-get? stories story-id) err-not-found)))
    (asserts! (is-authorized story-id tx-sender) err-unauthorized)
    (asserts! (is-eq (get status story) status-published) err-invalid-status)

    (try! (map-set story-interactions {story-id: story-id, user: tx-sender}
      {
        interaction-type: interaction-type,
        timestamp: stacks-block-height,
        comment-hash: comment-hash
      }
    ))

    ;; Update interaction count
    (try! (map-set stories story-id
      (merge story {
        interaction-count: (+ (get interaction-count story) u1)
      })
    ))

    (ok true)
  )
)

(define-public (connect-stories
  (story-id uint)
  (connected-story-id uint)
  (relationship-type (string-ascii 50))
  (description (string-ascii 300)))

  (let ((story (unwrap! (map-get? stories story-id) err-not-found))
        (connected-story (unwrap! (map-get? stories connected-story-id) err-not-found)))

    (asserts! (is-authorized story-id tx-sender) err-unauthorized)
    (asserts! (is-authorized connected-story-id tx-sender) err-unauthorized)
    (asserts! (not (is-eq story-id connected-story-id)) err-invalid-input)

    (try! (map-set story-connections {story-id: story-id, connected-story-id: connected-story-id}
      {
        relationship-type: relationship-type,
        description: description,
        created-by: tx-sender,
        created-at: stacks-block-height
      }
    ))

    (ok true)
  )
)

(define-public (endorse-cultural-significance
  (story-id uint)
  (heritage-value uint)
  (accuracy-rating uint))

  (let ((story (unwrap! (map-get? stories story-id) err-not-found))
        (current-sig (default-to
                       {heritage-value: u0, community-endorsements: u0, elder-approvals: u0, cultural-accuracy: u0, preservation-priority: u0}
                       (map-get? cultural-significance story-id))))

    (asserts! (is-authorized story-id tx-sender) err-unauthorized)
    (asserts! (<= heritage-value u10) err-invalid-input)
    (asserts! (<= accuracy-rating u10) err-invalid-input)

    (try! (map-set cultural-significance story-id
      (merge current-sig {
        heritage-value: (+ (get heritage-value current-sig) heritage-value),
        community-endorsements: (+ (get community-endorsements current-sig) u1),
        cultural-accuracy: (+ (get cultural-accuracy current-sig) accuracy-rating)
      })
    ))

    ;; Update story verification score
    (let ((new-score (calculate-verification-score story-id)))
      (try! (map-set stories story-id
        (merge story {
          verification-score: new-score,
          updated-at: stacks-block-height
        })
      ))
    )

    (ok true)
  )
)

;; ===============================================
;; PUBLIC FUNCTIONS - ADMIN
;; ===============================================

(define-public (feature-collection (collection-id uint))
  (let ((collection (unwrap! (map-get? collections collection-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)

    (try! (map-set collections collection-id
      (merge collection {is-featured: true})
    ))

    (ok true)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (asserts! (is-eq tx-sender contract-owner) err-owner-only)
  (asserts! (<= new-fee u1000) err-invalid-input) ;; Max 10%

  (var-set platform-fee new-fee)
  (ok true)
)

;; ===============================================
;; READ-ONLY FUNCTIONS
;; ===============================================

(define-read-only (get-story (story-id uint))
  (map-get? stories story-id)
)

(define-read-only (get-collection (collection-id uint))
  (map-get? collections collection-id)
)

(define-read-only (get-curator (curator principal))
  (map-get? curators curator)
)

(define-read-only (get-cultural-significance (story-id uint))
  (map-get? cultural-significance story-id)
)

(define-read-only (get-story-connections (story-id uint) (connected-story-id uint))
  (map-get? story-connections {story-id: story-id, connected-story-id: connected-story-id})
)

(define-read-only (has-story-access (story-id uint) (user principal))
  (is-authorized story-id user)
)

(define-read-only (get-verification-vote (story-id uint) (curator principal))
  (map-get? verification-votes {story-id: story-id, curator: curator})
)

(define-read-only (get-story-interaction (story-id uint) (user principal))
  (map-get? story-interactions {story-id: story-id, user: user})
)

(define-read-only (is-story-in-collection (collection-id uint) (story-id uint))
  (default-to false (map-get? collection-stories {collection-id: collection-id, story-id: story-id}))
)

(define-read-only (get-platform-stats)
  {
    total-stories: (var-get story-id-nonce),
    total-collections: (var-get collection-id-nonce),
    platform-fee: (var-get platform-fee),
    min-curator-stake: (var-get min-curator-stake)
  }
)
