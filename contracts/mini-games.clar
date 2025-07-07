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

(define-constant err-tournament-not-found (err u120))
(define-constant err-tournament-not-active (err u121))
(define-constant err-tournament-ended (err u122))
(define-constant err-already-joined-tournament (err u123))
(define-constant err-tournament-not-ended (err u124))

(define-data-var next-tournament-id uint u1)

(define-map tournaments uint
  {
    name: (string-ascii 50),
    game-id: uint,
    entry-fee: uint,
    prize-pool: uint,
    start-block: uint,
    end-block: uint,
    max-participants: uint,
    current-participants: uint,
    winner: (optional principal),
    active: bool
  }
)

(define-map tournament-participants { tournament-id: uint, player: principal }
  {
    best-score: uint,
    games-played: uint,
    entry-block: uint
  }
)

(define-map tournament-leaderboard { tournament-id: uint, rank: uint } principal)

(define-public (create-tournament (name (string-ascii 50)) (game-id uint) (entry-fee uint) (duration-blocks uint) (max-participants uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? games game-id)) err-game-not-found)
    (let ((tournament-id (var-get next-tournament-id))
          (start-block (+ stacks-block-height u1))
          (end-block (+ stacks-block-height duration-blocks)))
      (map-set tournaments tournament-id
        {
          name: name,
          game-id: game-id,
          entry-fee: entry-fee,
          prize-pool: u0,
          start-block: start-block,
          end-block: end-block,
          max-participants: max-participants,
          current-participants: u0,
          winner: none,
          active: true
        }
      )
      (var-set next-tournament-id (+ tournament-id u1))
      (ok tournament-id)
    )
  )
)

(define-public (join-tournament (tournament-id uint))
  (let ((tournament-info (unwrap! (map-get? tournaments tournament-id) err-tournament-not-found))
        (player-info (unwrap! (map-get? players tx-sender) err-not-registered))
        (participant-key { tournament-id: tournament-id, player: tx-sender }))
    (asserts! (get active tournament-info) err-tournament-not-active)
    (asserts! (>= stacks-block-height (get start-block tournament-info)) err-tournament-not-active)
    (asserts! (< stacks-block-height (get end-block tournament-info)) err-tournament-ended)
    (asserts! (< (get current-participants tournament-info) (get max-participants tournament-info)) err-tournament-ended)
    (asserts! (is-none (map-get? tournament-participants participant-key)) err-already-joined-tournament)
    
    (try! (stx-transfer? (get entry-fee tournament-info) tx-sender (as-contract tx-sender)))
    
    (map-set tournaments tournament-id
      (merge tournament-info {
        prize-pool: (+ (get prize-pool tournament-info) (get entry-fee tournament-info)),
        current-participants: (+ (get current-participants tournament-info) u1)
      })
    )
    
    (map-set tournament-participants participant-key
      {
        best-score: u0,
        games-played: u0,
        entry-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (play-tournament-game (tournament-id uint) (score uint))
  (let ((tournament-info (unwrap! (map-get? tournaments tournament-id) err-tournament-not-found))
        (participant-key { tournament-id: tournament-id, player: tx-sender })
        (participant-info (unwrap! (map-get? tournament-participants participant-key) err-not-registered))
        (game-id (get game-id tournament-info)))
    (asserts! (get active tournament-info) err-tournament-not-active)
    (asserts! (< stacks-block-height (get end-block tournament-info)) err-tournament-ended)
    
    (try! (play-game game-id score))
    
    (map-set tournament-participants participant-key
      (merge participant-info {
        best-score: (if (> score (get best-score participant-info)) score (get best-score participant-info)),
        games-played: (+ (get games-played participant-info) u1)
      })
    )
    (ok score)
  )
)

(define-public (end-tournament (tournament-id uint) (winner principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((tournament-info (unwrap! (map-get? tournaments tournament-id) err-tournament-not-found)))
      (asserts! (>= stacks-block-height (get end-block tournament-info)) err-tournament-not-ended)
      (asserts! (get active tournament-info) err-tournament-not-active)
      
      (try! (as-contract (stx-transfer? (get prize-pool tournament-info) tx-sender winner)))
      
      (map-set tournaments tournament-id
        (merge tournament-info {
          winner: (some winner),
          active: false
        })
      )
      (ok true)
    )
  )
)

(define-read-only (get-tournament-info (tournament-id uint))
  (map-get? tournaments tournament-id)
)

(define-read-only (get-tournament-participant (tournament-id uint) (player principal))
  (map-get? tournament-participants { tournament-id: tournament-id, player: player })
)


;; (impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-constant err-badge-not-found (err u130))
(define-constant err-badge-not-owned (err u131))
(define-constant err-badge-already-minted (err u132))

(define-data-var next-badge-id uint u1)

(define-map badge-types uint
  {
    name: (string-ascii 50),
    description: (string-ascii 100),
    image-uri: (string-ascii 200),
    achievement-id: uint,
    rarity: (string-ascii 20),
    mintable: bool
  }
)

(define-map badge-ownership uint principal)
(define-map badge-metadata uint (string-ascii 200))
(define-map player-badges principal (list 50 uint))
(define-map achievement-badge-mapping uint uint)

(define-public (create-badge-type (name (string-ascii 50)) (description (string-ascii 100)) (image-uri (string-ascii 200)) (achievement-id uint) (rarity (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? achievements achievement-id)) err-game-not-found)
    (let ((badge-type-id (var-get next-badge-id)))
      (map-set badge-types badge-type-id
        {
          name: name,
          description: description,
          image-uri: image-uri,
          achievement-id: achievement-id,
          rarity: rarity,
          mintable: true
        }
      )
      (map-set achievement-badge-mapping achievement-id badge-type-id)
      (var-set next-badge-id (+ badge-type-id u1))
      (ok badge-type-id)
    )
  )
)

(define-public (mint-achievement-badge (achievement-id uint))
  (let ((badge-type-id (unwrap! (map-get? achievement-badge-mapping achievement-id) err-badge-not-found))
        (badge-type (unwrap! (map-get? badge-types badge-type-id) err-badge-not-found))
        (player-info (unwrap! (map-get? players tx-sender) err-not-registered))
        (current-badges (default-to (list) (map-get? player-badges tx-sender))))
    (asserts! (get mintable badge-type) err-badge-not-found)
    (asserts! (is-none (index-of (get achievements-earned player-info) achievement-id)) err-badge-already-minted)
    
    (try! (claim-achievement achievement-id))
    
    (let ((badge-id (var-get next-badge-id)))
      (map-set badge-ownership badge-id tx-sender)
      (map-set badge-metadata badge-id (get image-uri badge-type))
      (map-set player-badges tx-sender 
        (unwrap! (as-max-len? (append current-badges badge-id) u50) err-reward-claim-failed))
      (var-set next-badge-id (+ badge-id u1))
      (ok badge-id)
    )
  )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-badge-not-owned)
    (asserts! (is-eq (some sender) (map-get? badge-ownership token-id)) err-badge-not-owned)
    (map-set badge-ownership token-id recipient)
    (let ((sender-badges (default-to (list) (map-get? player-badges sender)))
          (recipient-badges (default-to (list) (map-get? player-badges recipient))))
      (map-set player-badges sender 
        (filter remove-badge-id sender-badges))
      (map-set player-badges recipient
        (unwrap! (as-max-len? (append recipient-badges token-id) u50) err-reward-claim-failed))
    )
    (ok true)
  )
)

(define-private (remove-badge-id (badge-id uint))
  (not (is-eq badge-id badge-id))
)

(define-read-only (get-owner (token-id uint))
  (ok (map-get? badge-ownership token-id))
)

(define-read-only (get-last-token-id)
  (ok (- (var-get next-badge-id) u1))
)

(define-read-only (get-token-uri (token-id uint))
  (ok (map-get? badge-metadata token-id))
)

(define-read-only (get-badge-type (badge-type-id uint))
  (map-get? badge-types badge-type-id)
)

(define-read-only (get-player-badges (player principal))
  (map-get? player-badges player)
)

(define-read-only (get-achievement-badge (achievement-id uint))
  (map-get? achievement-badge-mapping achievement-id)
)

(define-constant err-season-not-found (err u140))
(define-constant err-season-not-active (err u141))
(define-constant err-season-ended (err u142))
(define-constant err-already-claimed-season-reward (err u143))
(define-constant err-rank-not-found (err u144))

(define-data-var current-season-id uint u1)
(define-data-var leaderboard-update-block uint u0)

(define-map seasons uint
  {
    name: (string-ascii 50),
    start-block: uint,
    end-block: uint,
    total-prize-pool: uint,
    participants: uint,
    active: bool,
    finalized: bool
  }
)

(define-map season-leaderboard { season-id: uint, rank: uint }
  {
    player: principal,
    total-score: uint,
    games-played: uint,
    last-updated: uint
  }
)

(define-map season-rewards uint
  {
    first-place: uint,
    second-place: uint,
    third-place: uint,
    top-ten: uint,
    participation: uint
  }
)

(define-map season-claims { season-id: uint, player: principal } bool)

(define-map player-season-stats { player: principal, season-id: uint }
  {
    total-score: uint,
    games-played: uint,
    current-rank: uint,
    last-updated: uint,
    reward-claimed: bool
  }
)

(define-public (create-season (name (string-ascii 50)) (duration-blocks uint) (prize-pool uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((season-id (var-get current-season-id))
          (start-block (+ stacks-block-height u1))
          (end-block (+ stacks-block-height duration-blocks)))
      (map-set seasons season-id
        {
          name: name,
          start-block: start-block,
          end-block: end-block,
          total-prize-pool: prize-pool,
          participants: u0,
          active: true,
          finalized: false
        }
      )
      (map-set season-rewards season-id
        {
          first-place: (/ (* prize-pool u40) u100),
          second-place: (/ (* prize-pool u25) u100),
          third-place: (/ (* prize-pool u15) u100),
          top-ten: (/ (* prize-pool u15) u100),
          participation: (/ (* prize-pool u5) u100)
        }
      )
      (var-set current-season-id (+ season-id u1))
      (ok season-id)
    )
  )
)

(define-public (update-leaderboard (player principal) (season-id uint))
  (let ((season-info (unwrap! (map-get? seasons season-id) err-season-not-found))
        (player-info (unwrap! (map-get? players player) err-not-registered))
        (player-season-key { player: player, season-id: season-id })
        (current-season-stats (default-to 
          { total-score: u0, games-played: u0, current-rank: u0, last-updated: u0, reward-claimed: false }
          (map-get? player-season-stats player-season-key))))
    (asserts! (get active season-info) err-season-not-active)
    (asserts! (< stacks-block-height (get end-block season-info)) err-season-ended)
    
    (map-set player-season-stats player-season-key
      {
        total-score: (get total-score player-info),
        games-played: (get games-played player-info),
        current-rank: u0,
        last-updated: stacks-block-height,
        reward-claimed: false
      }
    )
    
    (if (is-eq (get total-score current-season-stats) u0)
      (map-set seasons season-id
        (merge season-info { participants: (+ (get participants season-info) u1) }))
      true
    )
    
    (var-set leaderboard-update-block stacks-block-height)
    (ok true)
  )
)

(define-public (claim-season-reward (season-id uint))
  (let ((season-info (unwrap! (map-get? seasons season-id) err-season-not-found))
        (player-season-key { player: tx-sender, season-id: season-id })
        (player-stats (unwrap! (map-get? player-season-stats player-season-key) err-not-registered))
        (season-rewards-info (unwrap! (map-get? season-rewards season-id) err-season-not-found))
        (claim-key { season-id: season-id, player: tx-sender })
        (rank (get current-rank player-stats)))
    (asserts! (get finalized season-info) err-season-not-active)
    (asserts! (not (default-to false (map-get? season-claims claim-key))) err-already-claimed-season-reward)
    (asserts! (> rank u0) err-rank-not-found)
    
    (let ((reward-amount 
      (if (is-eq rank u1) (get first-place season-rewards-info)
        (if (is-eq rank u2) (get second-place season-rewards-info)
          (if (is-eq rank u3) (get third-place season-rewards-info)
            (if (<= rank u10) (get top-ten season-rewards-info)
              (get participation season-rewards-info)))))))
      (try! (as-contract (stx-transfer? reward-amount contract-owner tx-sender)))
      
      (map-set season-claims claim-key true)
      (map-set player-season-stats player-season-key
        (merge player-stats { reward-claimed: true }))
      (ok reward-amount)
    )
  )
)

(define-public (finalize-season (season-id uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((season-info (unwrap! (map-get? seasons season-id) err-season-not-found)))
      (asserts! (>= stacks-block-height (get end-block season-info)) err-season-not-active)
      (asserts! (not (get finalized season-info)) err-season-ended)
      
      (map-set seasons season-id
        (merge season-info { active: false, finalized: true }))
      (ok true)
    )
  )
)

(define-public (set-player-rank (season-id uint) (player principal) (rank uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((player-season-key { player: player, season-id: season-id })
          (player-stats (unwrap! (map-get? player-season-stats player-season-key) err-not-registered)))
      (map-set player-season-stats player-season-key
        (merge player-stats { current-rank: rank }))
      
      (map-set season-leaderboard { season-id: season-id, rank: rank }
        {
          player: player,
          total-score: (get total-score player-stats),
          games-played: (get games-played player-stats),
          last-updated: stacks-block-height
        }
      )
      (ok true)
    )
  )
)

(define-read-only (get-season-info (season-id uint))
  (map-get? seasons season-id)
)

(define-read-only (get-season-leaderboard (season-id uint) (rank uint))
  (map-get? season-leaderboard { season-id: season-id, rank: rank })
)

(define-read-only (get-player-season-stats (player principal) (season-id uint))
  (map-get? player-season-stats { player: player, season-id: season-id })
)

(define-read-only (get-season-rewards (season-id uint))
  (map-get? season-rewards season-id)
)

(define-read-only (get-current-season)
  (ok (- (var-get current-season-id) u1))
)

(define-read-only (check-season-reward-eligibility (player principal) (season-id uint))
  (let ((claim-key { season-id: season-id, player: player })
        (player-stats (map-get? player-season-stats { player: player, season-id: season-id })))
    (if (and (is-some player-stats) 
             (> (get current-rank (unwrap-panic player-stats)) u0)
             (not (default-to false (map-get? season-claims claim-key))))
      (ok true)
      (ok false)
    )
  )
)