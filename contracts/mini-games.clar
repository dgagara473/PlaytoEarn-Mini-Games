(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-game-not-found (err u103))
(define-constant err-insufficient-fee (err u104))
(define-constant err-invalid-score (err u105))
(define-constant err-reward-claim-failed (err u106))
(define-constant err-already-claimed (err u107))
(define-constant err-not-eligible (err u108))

(define-data-var registration-fee uint u1000000) ;; 1 STX
(define-data-var next-game-id uint u1)
(define-data-var next-achievement-id uint u1)
(define-data-var total-stx-rewards uint u0)

(define-map players principal 
  {
    registered: bool,
    total-score: uint,
    games-played: uint,
    achievements-earned: (list 20 uint),
    rewards-claimed: uint
  }
)

(define-map games uint 
  {
    name: (string-ascii 50),
    fee: uint,
    active: bool,
    high-score: uint,
    high-scorer: (optional principal),
    plays: uint
  }
)

(define-map achievements uint 
  {
    name: (string-ascii 50),
    description: (string-ascii 100),
    reward-amount: uint,
    game-id: uint,
    score-threshold: uint
  }
)

(define-map player-game-data { player: principal, game-id: uint }
  {
    high-score: uint,
    plays: uint,
    last-played: uint,
    rewards-claimed: uint
  }
)

(define-map achievement-claims { player: principal, achievement-id: uint } bool)

(define-public (register-player)
  (let ((fee (var-get registration-fee)))
    (asserts! (not (default-to false (get registered (map-get? players tx-sender)))) err-already-registered)
    (try! (stx-transfer? fee tx-sender contract-owner))
    (ok (map-set players tx-sender 
      {
        registered: true,
        total-score: u0,
        games-played: u0,
        achievements-earned: (list),
        rewards-claimed: u0
      }
    ))
  )
)

(define-public (add-game (name (string-ascii 50)) (fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((game-id (var-get next-game-id)))
      (map-set games game-id
        {
          name: name,
          fee: fee,
          active: true,
          high-score: u0,
          high-scorer: none,
          plays: u0
        }
      )
      (var-set next-game-id (+ game-id u1))
      (ok game-id)
    )
  )
)

(define-public (add-achievement (name (string-ascii 50)) (description (string-ascii 100)) (reward-amount uint) (game-id uint) (score-threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? games game-id)) err-game-not-found)
    (let ((achievement-id (var-get next-achievement-id)))
      (map-set achievements achievement-id
        {
          name: name,
          description: description,
          reward-amount: reward-amount,
          game-id: game-id,
          score-threshold: score-threshold
        }
      )
      (var-set next-achievement-id (+ achievement-id u1))
      (var-set total-stx-rewards (+ (var-get total-stx-rewards) reward-amount))
      (ok achievement-id)
    )
  )
)

(define-public (play-game (game-id uint) (score uint))
  (let (
    (player-info (unwrap! (map-get? players tx-sender) err-not-registered))
    (game-info (unwrap! (map-get? games game-id) err-game-not-found))
    (game-fee (get fee game-info))
    (player-game-key { player: tx-sender, game-id: game-id })
    (player-game-info (default-to { high-score: u0, plays: u0, last-played: u0, rewards-claimed: u0 } (map-get? player-game-data player-game-key)))
    (current-stacks-block-height stacks-block-height)
  )
    (asserts! (get active game-info) err-game-not-found)
    (asserts! (>= score u0) err-invalid-score)
    (try! (stx-transfer? game-fee tx-sender contract-owner))
    
    ;; Update player game data
    (map-set player-game-data player-game-key
      (merge player-game-info {
        high-score: (if (> score (get high-score player-game-info)) score (get high-score player-game-info)),
        plays: (+ (get plays player-game-info) u1),
        last-played: current-stacks-block-height
      })
    )
    
    ;; Update global game data
    (map-set games game-id
      (merge game-info {
        high-score: (if (> score (get high-score game-info)) score (get high-score game-info)),
        high-scorer: (if (> score (get high-score game-info)) (some tx-sender) (get high-scorer game-info)),
        plays: (+ (get plays game-info) u1)
      })
    )
    
    ;; Update player data
    (map-set players tx-sender
      (merge player-info {
        total-score: (+ (get total-score player-info) score),
        games-played: (+ (get games-played player-info) u1)
      })
    )
    
    (ok score)
  )
)

(define-public (claim-achievement (achievement-id uint))
  (let (
    (player-info (unwrap! (map-get? players tx-sender) err-not-registered))
    (achievement-info (unwrap! (map-get? achievements achievement-id) err-game-not-found))
    (game-id (get game-id achievement-info))
    (player-game-key { player: tx-sender, game-id: game-id })
    (player-game-info (unwrap! (map-get? player-game-data player-game-key) err-not-registered))
    (claim-key { player: tx-sender, achievement-id: achievement-id })
    (already-claimed (default-to false (map-get? achievement-claims claim-key)))
  )
    (asserts! (not already-claimed) err-already-claimed)
    (asserts! (>= (get high-score player-game-info) (get score-threshold achievement-info)) err-not-eligible)
    
    ;; Transfer reward
    (try! (as-contract (stx-transfer? (get reward-amount achievement-info) contract-owner tx-sender)))
    
    ;; Update player achievements
    (map-set players tx-sender
      (merge player-info {
        achievements-earned: (unwrap! (as-max-len? (append (get achievements-earned player-info) achievement-id) u20) err-reward-claim-failed),
        rewards-claimed: (+ (get rewards-claimed player-info) (get reward-amount achievement-info))
      })
    )
    
    ;; Mark achievement as claimed
    (map-set achievement-claims claim-key true)
    
    (ok (get reward-amount achievement-info))
  )
)

(define-public (deactivate-game (game-id uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((game-info (unwrap! (map-get? games game-id) err-game-not-found)))
      (map-set games game-id
        (merge game-info { active: false })
      )
      (ok true)
    )
  )
)

(define-public (update-registration-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set registration-fee new-fee)
    (ok true)
  )
)

(define-read-only (get-player-info (player principal))
  (map-get? players player)
)

(define-read-only (get-game-info (game-id uint))
  (map-get? games game-id)
)

(define-read-only (get-achievement-info (achievement-id uint))
  (map-get? achievements achievement-id)
)

(define-read-only (get-player-game-stats (player principal) (game-id uint))
  (map-get? player-game-data { player: player, game-id: game-id })
)

(define-read-only (check-achievement-eligibility (player principal) (achievement-id uint))
  (let (
    (achievement-info (unwrap! (map-get? achievements achievement-id) (err u1)))
    (game-id (get game-id achievement-info))
    (player-game-info (unwrap! (map-get? player-game-data { player: player, game-id: game-id }) (err u1)))
    (claim-key { player: player, achievement-id: achievement-id })
    (already-claimed (default-to false (map-get? achievement-claims claim-key)))
  )
    (if (and (not already-claimed) (>= (get high-score player-game-info) (get score-threshold achievement-info)))
      (ok true)
      (ok false)
    )
  )
)



(define-constant referral-reward-percentage u20)
(define-constant err-invalid-referrer (err u110))

(define-map referral-stats principal 
  {
    total-referrals: uint,
    rewards-earned: uint
  }
)

(define-public (register-with-referral (referrer principal))
  (let ((fee (var-get registration-fee))
        (referrer-info (map-get? players referrer))
        (reward-amount (/ (* fee referral-reward-percentage) u100)))
    (asserts! (not (default-to false (get registered (map-get? players tx-sender)))) err-already-registered)
    (asserts! (is-some referrer-info) err-invalid-referrer)
    (try! (stx-transfer? fee tx-sender contract-owner))
    (try! (as-contract (stx-transfer? reward-amount contract-owner referrer)))
    
    (map-set referral-stats referrer
      (merge (default-to {total-referrals: u0, rewards-earned: u0} (map-get? referral-stats referrer))
        {
          total-referrals: (+ (default-to u0 (get total-referrals (map-get? referral-stats referrer))) u1),
          rewards-earned: (+ (default-to u0 (get rewards-earned (map-get? referral-stats referrer))) reward-amount)
        }
      )
    )
    
    (ok (map-set players tx-sender 
      {
        registered: true,
        total-score: u0,
        games-played: u0,
        achievements-earned: (list),
        rewards-claimed: u0
      }
    ))
  )
)

(define-read-only (get-referral-stats (player principal))
  (map-get? referral-stats player)
)


(define-constant min-stake-amount u1000000)
(define-constant stake-fee-percentage u5)
(define-constant err-insufficient-stake (err u111))

(define-map staking-pools uint
  {
    total-staked: uint,
    rewards-accumulated: uint,
    stakers-count: uint
  }
)

(define-map staker-positions { game-id: uint, staker: principal }
  {
    amount: uint,
    rewards-claimed: uint,
    last-claim: uint
  }
)

(define-public (stake-in-game (game-id uint) (amount uint))
  (let ((game-info (unwrap! (map-get? games game-id) err-game-not-found))
        (pool (default-to {total-staked: u0, rewards-accumulated: u0, stakers-count: u0} 
               (map-get? staking-pools game-id))))
    (asserts! (>= amount min-stake-amount) err-insufficient-stake)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set staking-pools game-id
      (merge pool {
        total-staked: (+ (get total-staked pool) amount),
        stakers-count: (+ (get stakers-count pool) u1)
      })
    )
    
    (map-set staker-positions {game-id: game-id, staker: tx-sender}
      {
        amount: amount,
        rewards-claimed: u0,
        last-claim: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (claim-staking-rewards (game-id uint))
  (let ((position (unwrap! (map-get? staker-positions {game-id: game-id, staker: tx-sender}) err-not-registered))
        (pool (unwrap! (map-get? staking-pools game-id) err-game-not-found))
        (reward-share (/ (* (get rewards-accumulated pool) (get amount position)) (get total-staked pool))))
    (try! (as-contract (stx-transfer? reward-share tx-sender tx-sender)))
    
    (map-set staker-positions {game-id: game-id, staker: tx-sender}
      (merge position {
        rewards-claimed: (+ (get rewards-claimed position) reward-share),
        last-claim: stacks-block-height
      })
    )
    (ok reward-share)
  )
)