;; Daily Challenge System
;; Provides time-limited challenges with special rewards and streak bonuses

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_CHALLENGE_NOT_FOUND (err u401))
(define-constant ERR_CHALLENGE_EXPIRED (err u402))
(define-constant ERR_CHALLENGE_ALREADY_COMPLETED (err u403))
(define-constant ERR_PLAYER_NOT_REGISTERED (err u404))
(define-constant ERR_INVALID_CHALLENGE_TYPE (err u405))
(define-constant ERR_INSUFFICIENT_PROGRESS (err u406))
(define-constant ERR_CHALLENGE_NOT_ACTIVE (err u407))
(define-constant ERR_STREAK_ALREADY_CLAIMED (err u408))

(define-data-var next-challenge-id uint u1)
(define-data-var daily-challenge-duration uint u1440) ;; 24 hours in blocks
(define-data-var challenge-base-reward uint u100000) ;; 0.1 STX base reward

;; Store daily challenge definitions
(define-map daily-challenges
    uint
    {
        challenge-id: uint,
        title: (string-ascii 50),
        description: (string-ascii 200),
        challenge-type: (string-ascii 30), ;; "score", "games", "achievement", "streak"
        target-value: uint,
        game-id: (optional uint), ;; Specific game or none for global
        reward-amount: uint,
        bonus-energy: uint,
        creation-date: uint,
        expiry-date: uint,
        active: bool,
        completion-count: uint
    }
)

;; Track player challenge progress
(define-map player-challenge-progress
    { player: principal, challenge-id: uint }
    {
        current-progress: uint,
        completion-status: bool,
        start-date: uint,
        completion-date: (optional uint),
        reward-claimed: bool
    }
)

;; Track daily streak information
(define-map player-daily-streaks
    principal
    {
        current-streak: uint,
        longest-streak: uint,
        last-challenge-date: uint,
        total-challenges-completed: uint,
        streak-rewards-claimed: uint
    }
)

;; Store challenge templates for auto-generation
(define-map challenge-templates
    uint
    {
        template-name: (string-ascii 50),
        challenge-type: (string-ascii 30),
        base-target: uint,
        target-scaling: uint, ;; Multiplier for difficulty
        base-reward: uint,
        reward-scaling: uint,
        energy-bonus: uint
    }
)

;; Create a new daily challenge
(define-public (create-daily-challenge
    (title (string-ascii 50))
    (description (string-ascii 200))
    (challenge-type (string-ascii 30))
    (target-value uint)
    (game-id (optional uint))
    (reward-amount uint)
    (bonus-energy uint))
    (let
        (
            (challenge-id (var-get next-challenge-id))
            (expiry-date (+ stacks-block-height (var-get daily-challenge-duration)))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> target-value u0) ERR_INVALID_CHALLENGE_TYPE)
        
        (map-set daily-challenges challenge-id {
            challenge-id: challenge-id,
            title: title,
            description: description,
            challenge-type: challenge-type,
            target-value: target-value,
            game-id: game-id,
            reward-amount: reward-amount,
            bonus-energy: bonus-energy,
            creation-date: stacks-block-height,
            expiry-date: expiry-date,
            active: true,
            completion-count: u0
        })
        
        (var-set next-challenge-id (+ challenge-id u1))
        (ok challenge-id)
    )
)

;; Start a challenge (player participation)
(define-public (start-challenge (challenge-id uint))
    (let
        (
            (challenge-info (unwrap! (map-get? daily-challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
            (player-info (unwrap! (contract-call? .mini-games get-player-info tx-sender) ERR_PLAYER_NOT_REGISTERED))
            (existing-progress (map-get? player-challenge-progress { player: tx-sender, challenge-id: challenge-id }))
        )
        (asserts! (get active challenge-info) ERR_CHALLENGE_NOT_ACTIVE)
        (asserts! (< stacks-block-height (get expiry-date challenge-info)) ERR_CHALLENGE_EXPIRED)
        (asserts! (is-none existing-progress) ERR_CHALLENGE_ALREADY_COMPLETED)
        
        (map-set player-challenge-progress { player: tx-sender, challenge-id: challenge-id } {
            current-progress: u0,
            completion-status: false,
            start-date: stacks-block-height,
            completion-date: none,
            reward-claimed: false
        })
        
        (ok true)
    )
)

;; Update challenge progress (called after game activities)
(define-public (update-challenge-progress (player principal) (challenge-id uint) (progress-value uint))
    (let
        (
            (challenge-info (unwrap! (map-get? daily-challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
            (progress-info (unwrap! (map-get? player-challenge-progress { player: player, challenge-id: challenge-id }) ERR_CHALLENGE_NOT_FOUND))
        )
        (asserts! (get active challenge-info) ERR_CHALLENGE_NOT_ACTIVE)
        (asserts! (< stacks-block-height (get expiry-date challenge-info)) ERR_CHALLENGE_EXPIRED)
        (asserts! (not (get completion-status progress-info)) ERR_CHALLENGE_ALREADY_COMPLETED)
        
        (let 
            (
                (new-progress (+ (get current-progress progress-info) progress-value))
                (challenge-completed (>= new-progress (get target-value challenge-info)))
            )
            (map-set player-challenge-progress { player: player, challenge-id: challenge-id }
                (merge progress-info {
                    current-progress: new-progress,
                    completion-status: challenge-completed,
                    completion-date: (if challenge-completed (some stacks-block-height) none)
                }))
            
            ;; Update challenge completion count if completed
            (if challenge-completed
                (begin
                    (map-set daily-challenges challenge-id
                        (merge challenge-info { completion-count: (+ (get completion-count challenge-info) u1) }))
                    (unwrap-panic (update-player-streak player))
                )
                true
            )
            
            (ok challenge-completed)
        )
    )
)

;; Claim challenge reward
(define-public (claim-challenge-reward (challenge-id uint))
    (let
        (
            (challenge-info (unwrap! (map-get? daily-challenges challenge-id) ERR_CHALLENGE_NOT_FOUND))
            (progress-info (unwrap! (map-get? player-challenge-progress { player: tx-sender, challenge-id: challenge-id }) ERR_CHALLENGE_NOT_FOUND))
        )
        (asserts! (get completion-status progress-info) ERR_INSUFFICIENT_PROGRESS)
        (asserts! (not (get reward-claimed progress-info)) ERR_CHALLENGE_ALREADY_COMPLETED)
        
        ;; Transfer STX reward
        (try! (as-contract (stx-transfer? (get reward-amount challenge-info) CONTRACT_OWNER tx-sender)))
        
        ;; Grant bonus energy if specified
        (if (> (get bonus-energy challenge-info) u0)
            (try! (contract-call? .mini-games grant-energy-boost tx-sender "free-refills" (get bonus-energy challenge-info)))
            true
        )
        
        ;; Mark reward as claimed
        (map-set player-challenge-progress { player: tx-sender, challenge-id: challenge-id }
            (merge progress-info { reward-claimed: true }))
        
        (ok (get reward-amount challenge-info))
    )
)

;; Update player's daily streak
(define-private (update-player-streak (player principal))
    (let
        (
            (current-day (/ stacks-block-height u1440)) ;; Approximate day number
            (streak-data (default-to 
                {
                    current-streak: u0,
                    longest-streak: u0,
                    last-challenge-date: u0,
                    total-challenges-completed: u0,
                    streak-rewards-claimed: u0
                }
                (map-get? player-daily-streaks player)
            ))
            (last-day (/ (get last-challenge-date streak-data) u1440))
            (is-consecutive (is-eq (- current-day last-day) u1))
            (new-streak (if is-consecutive 
                          (+ (get current-streak streak-data) u1)
                          u1))
        )
        (map-set player-daily-streaks player
            (merge streak-data {
                current-streak: new-streak,
                longest-streak: (if (> new-streak (get longest-streak streak-data)) 
                                  new-streak 
                                  (get longest-streak streak-data)),
                last-challenge-date: stacks-block-height,
                total-challenges-completed: (+ (get total-challenges-completed streak-data) u1)
            }))
        (ok true)
    )
)

;; Claim streak bonus rewards
(define-public (claim-streak-bonus)
    (let
        (
            (streak-data (unwrap! (map-get? player-daily-streaks tx-sender) ERR_PLAYER_NOT_REGISTERED))
            (current-streak (get current-streak streak-data))
            (streak-reward (calculate-streak-reward current-streak))
        )
        (asserts! (>= current-streak u3) ERR_INSUFFICIENT_PROGRESS) ;; Minimum 3-day streak
        (asserts! (> streak-reward u0) ERR_INSUFFICIENT_PROGRESS)
        
        ;; Transfer streak bonus
        (try! (as-contract (stx-transfer? streak-reward CONTRACT_OWNER tx-sender)))
        
        ;; Update claimed amount
        (map-set player-daily-streaks tx-sender
            (merge streak-data { streak-rewards-claimed: (+ (get streak-rewards-claimed streak-data) streak-reward) }))
        
        (ok streak-reward)
    )
)

;; Calculate streak bonus reward
(define-private (calculate-streak-reward (streak-days uint))
    (if (>= streak-days u30) u3000000 ;; 30+ days = 3 STX
        (if (>= streak-days u14) u1500000 ;; 14+ days = 1.5 STX
            (if (>= streak-days u7) u750000 ;; 7+ days = 0.75 STX
                (if (>= streak-days u3) u300000 ;; 3+ days = 0.3 STX
                    u0))))
)

;; Create challenge template for auto-generation
(define-public (create-challenge-template
    (template-name (string-ascii 50))
    (challenge-type (string-ascii 30))
    (base-target uint)
    (target-scaling uint)
    (base-reward uint)
    (energy-bonus uint))
    (let
        (
            (template-id (var-get next-challenge-id))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        (map-set challenge-templates template-id {
            template-name: template-name,
            challenge-type: challenge-type,
            base-target: base-target,
            target-scaling: target-scaling,
            base-reward: base-reward,
            reward-scaling: u0,
            energy-bonus: energy-bonus
        })
        
        (ok template-id)
    )
)

;; Read-only functions
(define-read-only (get-challenge-info (challenge-id uint))
    (map-get? daily-challenges challenge-id)
)

(define-read-only (get-player-challenge-progress (player principal) (challenge-id uint))
    (map-get? player-challenge-progress { player: player, challenge-id: challenge-id })
)

(define-read-only (get-player-streak-info (player principal))
    (map-get? player-daily-streaks player)
)

(define-read-only (get-active-challenges)
    {
        current-block: stacks-block-height,
        total-challenges: (var-get next-challenge-id),
        daily-duration: (var-get daily-challenge-duration),
        base-reward: (var-get challenge-base-reward)
    }
)

(define-read-only (get-challenge-leaderboard (challenge-id uint))
    ;; Simplified leaderboard for challenge completions
    {
        challenge-id: challenge-id,
        total-completions: (match (map-get? daily-challenges challenge-id)
            challenge-data (get completion-count challenge-data)
            u0),
        completion-rate: u0 ;; Simplified for demo
    }
)

(define-read-only (calculate-streak-multiplier (streak-days uint))
    (if (>= streak-days u30) u300 ;; 3x multiplier for 30+ day streak
        (if (>= streak-days u14) u200 ;; 2x multiplier for 14+ day streak
            (if (>= streak-days u7) u150 ;; 1.5x multiplier for 7+ day streak
                (if (>= streak-days u3) u125 ;; 1.25x multiplier for 3+ day streak
                    u100)))) ;; Base multiplier
)

(define-read-only (get-player-daily-stats (player principal))
    (match (map-get? player-daily-streaks player)
        streak-data
            {
                current-streak: (get current-streak streak-data),
                longest-streak: (get longest-streak streak-data),
                total-completed: (get total-challenges-completed streak-data),
                rewards-earned: (get streak-rewards-claimed streak-data),
                next-streak-bonus: (calculate-streak-reward (+ (get current-streak streak-data) u1))
            }
        {
            current-streak: u0,
            longest-streak: u0,
            total-completed: u0,
            rewards-earned: u0,
            next-streak-bonus: u0
        }
    )
)

(define-read-only (is-challenge-expired (challenge-id uint))
    (match (map-get? daily-challenges challenge-id)
        challenge-data (>= stacks-block-height (get expiry-date challenge-data))
        true)
)

(define-read-only (get-next-challenge-id)
    (var-get next-challenge-id)
)
