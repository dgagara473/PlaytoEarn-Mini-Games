# PlaytoEarn Mini Games Smart Contract

This Clarity smart contract implements a play-to-earn gaming platform on the Stacks blockchain where players can earn STX tokens for achievements in mini-games.

## Features

- Player registration system
- Multiple mini-games with configurable parameters
- Achievement system with STX rewards
- High score tracking
- Game statistics

## Contract Functions

### Player Management

- `register-player`: Register a new player by paying a registration fee
- `get-player-info`: Get information about a player

### Game Management

- `add-game`: Add a new game (owner only)
- `play-game`: Play a game by paying the game fee and submitting a score
- `deactivate-game`: Deactivate a game (owner only)
- `get-game-info`: Get information about a game

### Achievement System

- `add-achievement`: Add a new achievement with STX rewards (owner only)
- `claim-achievement`: Claim STX rewards for completing an achievement
- `get-achievement-info`: Get information about an achievement
- `check-achievement-eligibility`: Check if a player is eligible for an achievement

### Administration

- `update-registration-fee`: Update the player registration fee (owner only)

## Usage Examples

### Register as a player

```clarity
(contract-call? .mini-games register-player)
```

### Add a new game (contract owner only)

```clarity
(contract-call? .mini-games add-game "Space Shooter" u100000)
```

### Add an achievement (contract owner only)

```clarity
(contract-call? .mini-games add-achievement "Space Master" "Score over 1000 points in Space Shooter" u5000000 u1 u1000)
```

### Play a game

```clarity
(contract-call? .mini-games play-game u1 u1250)
```

### Claim an achievement reward

```clarity
(contract-call? .mini-games claim-achievement u1)
```

## Implementation Notes

- Registration fee and game fees are paid in STX
- Achievements are tied to specific games and score thresholds
- Players can only claim each achievement once
- The contract owner receives all game and registration fees
- Players receive STX rewards directly from the contract

## Security Considerations

- The contract owner has significant control over the platform
- Players should verify game and achievement parameters before participating
- Achievement rewards are locked in the contract when created