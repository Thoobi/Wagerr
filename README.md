# Wagerr - Decentralized Betting Platform

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)]()
[![Version](https://img.shields.io/badge/version-v2.0.0-orange.svg)]()
[![Stacks](https://img.shields.io/badge/blockchain-Stacks-purple.svg)](https://stacks.co)
[![Clarity](https://img.shields.io/badge/smart%20contracts-Clarity-red.svg)](https://clarity-lang.org)

## Overview

Wagerr is a revolutionary blockchain-based decentralized betting platform that eliminates intermediaries and transforms the traditional sportsbook industry. Built on the Stacks blockchain using Clarity smart contracts, Wagerr provides a secure, transparent, and globally accessible betting experience where users can place bets directly against the blockchain.

### Key Advantages

- **🔒 Complete Privacy**: No registration or KYC requirements
- **⚡ Guaranteed Payouts**: Trustless smart contract execution
- **💰 Lower Fees**: 2-6% vs traditional 10-20% margins
- **🌍 Global Access**: Available worldwide without restrictions
- **🔥 Deflationary Model**: Token burns create scarcity and value
- **📱 Cross-Platform**: Mobile and desktop applications

## Table of Contents

- [Architecture](#architecture)
- [Smart Contracts](#smart-contracts)
- [Token Economics](#token-economics)
- [Oracle System](#oracle-system)
- [Installation](#installation)
- [Usage](#usage)
- [API Reference](#api-reference)
- [Development](#development)
- [Contributing](#contributing)
- [Security](#security)
- [Roadmap](#roadmap)
- [License](#license)

## Architecture

Wagerr operates on a decentralized architecture consisting of:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   User Layer    │    │  Application    │    │   Blockchain    │
│                 │    │     Layer       │    │     Layer       │
│ • Mobile App    │◄──►│ • Web Frontend  │◄──►│ • Stacks Chain  │
│ • Desktop App   │    │ • API Gateway   │    │ • Clarity SC    │
│ • Web Interface │    │ • Backend APIs  │    │ • Bitcoin L1    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                │
                                ▼
                       ┌─────────────────┐
                       │ Oracle Network  │
                       │                 │
                       │ • Masternodes   │
                       │ • Data Feeds    │
                       │ • Result Validation │
                       └─────────────────┘
```

### Core Components

1. **Clarity Smart Contracts**: Handle betting logic, payouts, and token operations
2. **Oracle Masternodes**: Validate game results and execute automated payouts
3. **WGR Token**: Native utility token for all platform operations
4. **Decentralized Frontend**: Web and mobile applications for user interaction

## Smart Contracts

Wagerr utilizes Clarity smart contracts deployed on the Stacks blockchain for maximum security and transparency.

### Contract Architecture

```clarity
;; Core Contracts
contracts/
├── betting-core.clar          ; Main betting logic
├── token-management.clar      ; WGR token operations
├── oracle-system.clar         ; Oracle validation and rewards
├── masternode-rewards.clar    ; Staking and reward distribution
└── governance.clar            ; Protocol governance
```

### Key Contract Functions

#### Betting Core Contract (`betting-core.clar`)

```clarity
;; Place a bet
(define-public (place-bet 
  (event-id uint) 
  (bet-amount uint) 
  (selection uint) 
  (odds uint))
  ;; Implementation
)

;; Settle bet results
(define-public (settle-bet 
  (bet-id uint) 
  (result uint))
  ;; Implementation with oracle validation
)
```

#### Token Management Contract (`token-management.clar`)

```clarity
;; Burn tokens from losing bets
(define-private (burn-losing-bet-tokens (amount uint))
  ;; Deflationary mechanism
)

;; Mint rewards for winning bets
(define-private (mint-winning-rewards (amount uint))
  ;; Controlled token minting
)
```

### Contract Deployment

```bash
# Deploy to testnet
clarinet deploy --testnet

# Deploy to mainnet
clarinet deploy --mainnet
```

## Token Economics

### WGR Token Specifications

- **Token Name**: Wagerr (WGR)
- **Blockchain**: Stacks
- **Standard**: SIP-010 Fungible Token
- **Total Supply**: Dynamic (deflationary model)
- **Decimals**: 8

### Economic Model

```
Betting Activity Flow:
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ User Places │───►│   Bet       │───►│   Result    │
│    Bet      │    │ Validation  │    │ Settlement  │
└─────────────┘    └─────────────┘    └─────────────┘
                                              │
                          ┌───────────────────┼───────────────────┐
                          ▼                   ▼                   ▼
                   ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
                   │ Losing Bets │    │ Oracle Fees │    │ Token Burns │
                   │    Burn     │    │(2-6% of vol)│    │(Deflation)  │
                   │   Tokens    │    │             │    │             │
                   └─────────────┘    └─────────────┘    └─────────────┘
```

### Fee Structure

| Activity | Fee Range | Distribution |
|----------|-----------|--------------|
| Sports Betting | 2-4% | 50% Oracle Rewards, 30% Burns, 20% Development |
| Esports Betting | 3-5% | 45% Oracle Rewards, 35% Burns, 20% Development |
| Special Events | 4-6% | 40% Oracle Rewards, 40% Burns, 20% Development |

## Oracle System

### Oracle Masternodes

Oracle Masternodes form the backbone of Wagerr's decentralized data validation system:

#### Requirements
- **Minimum Stake**: 25,000 WGR tokens
- **Hardware**: VPS with 2+ CPU cores, 4GB RAM, 50GB storage
- **Uptime**: 99%+ availability required
- **Network**: Stable internet connection

#### Responsibilities
1. **Data Validation**: Verify sports results from multiple sources
2. **Consensus Participation**: Vote on disputed outcomes
3. **Smart Contract Execution**: Trigger automated payouts
4. **Network Security**: Maintain blockchain integrity

#### Reward Structure
```
Oracle Rewards = (Betting Volume × Fee Rate × Oracle Share) + PoS Rewards
Daily Rewards = Base Rewards + Performance Bonuses + Uptime Bonuses
```

### Oracle Data Flow

```
External Data Sources ──► Oracle Masternodes ──► Consensus Mechanism ──► Smart Contract Execution
        │                         │                        │                      │
        ▼                         ▼                        ▼                      ▼
   ESPN, Reuters           Data Aggregation         Super-Majority Vote      Automated Payouts
   Official APIs           & Validation             (67%+ agreement)         & Token Burns
```

## Installation

### Prerequisites

- Node.js 16.0+
- Clarinet CLI
- Stacks Wallet
- Git

### Quick Start

```bash
# Clone the repository
git clone https://github.com/wagerr/wagerr-platform.git
cd wagerr-platform

# Install dependencies
npm install

# Install Clarinet (Clarity development tool)
curl -L https://github.com/hirosystems/clarinet/releases/download/v1.5.4/clarinet-linux-x64.tar.gz | tar xz
sudo mv clarinet /usr/local/bin

# Initialize development environment
clarinet new wagerr-contracts
cd wagerr-contracts

# Test smart contracts
clarinet test

# Start local development server
npm run dev
```

### Mobile App Installation

#### Android
```bash
# Download from releases or build from source
git clone https://github.com/wagerr/wagerr-mobile.git
cd wagerr-mobile
npm install
npx react-native run-android
```

#### Desktop Application
```bash
# Electron-based desktop app
git clone https://github.com/wagerr/wagerr-desktop.git
cd wagerr-desktop
npm install
npm run build
npm run start
```

## Usage

### For Bettors

#### 1. Wallet Setup
```javascript
// Connect to Stacks wallet
import { openContractCall } from '@stacks/connect';

const connectWallet = async () => {
  const response = await openContractCall({
    network: 'mainnet',
    contractAddress: 'SP2C2YFP12AJZB4MABJBAJ55XECVS7E4PMMZ89YZR',
    contractName: 'wagerr-betting-core',
    functionName: 'get-user-balance'
  });
};
```

#### 2. Place Bet
```javascript
// Place a bet on upcoming game
const placeBet = async (eventId, amount, selection) => {
  const response = await openContractCall({
    contractAddress: 'SP2C2YFP12AJZB4MABJBAJ55XECVS7E4PMMZ89YZR',
    contractName: 'wagerr-betting-core',
    functionName: 'place-bet',
    functionArgs: [
      uintCV(eventId),
      uintCV(amount * 100000000), // Convert to smallest unit
      uintCV(selection),
      uintCV(odds)
    ]
  });
};
```

#### 3. Check Results
```javascript
// Monitor bet status
const checkBetStatus = async (betId) => {
  const result = await fetch(`/api/bets/${betId}/status`);
  return result.json();
};
```

### For Oracle Operators

#### 1. Masternode Setup
```bash
# Install masternode software
curl -L https://releases.wagerr.com/masternode/install.sh | bash

# Configure masternode
wagerr-masternode init --stake=25000

# Start masternode
wagerr-masternode start --network=mainnet
```

#### 2. Oracle Configuration
```yaml
# oracle-config.yml
network: mainnet
min_confidence: 0.75
data_sources:
  - espn_api
  - reuters_sports
  - official_league_apis
consensus_threshold: 0.67
reward_address: "SP1..."
```

### For Developers

#### 1. Smart Contract Development
```clarity
;; Example betting contract function
(define-public (create-betting-event 
  (event-name (string-ascii 64))
  (start-time uint)
  (end-time uint)
  (options (list 10 (string-ascii 32))))
  
  (let ((event-id (+ (var-get last-event-id) u1)))
    (map-set betting-events event-id {
      name: event-name,
      start: start-time,
      end: end-time,
      options: options,
      status: "active"
    })
    (var-set last-event-id event-id)
    (ok event-id)))
```

#### 2. API Integration
```javascript
// Wagerr API client
const WagerrAPI = {
  async getActiveEvents() {
    return fetch('/api/v1/events/active').then(r => r.json());
  },
  
  async getOdds(eventId) {
    return fetch(`/api/v1/events/${eventId}/odds`).then(r => r.json());
  },
  
  async placeBet(betData) {
    return fetch('/api/v1/bets', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(betData)
    });
  }
};
```

## API Reference

### REST API Endpoints

#### Events
```
GET    /api/v1/events              # List all events
GET    /api/v1/events/active       # Active betting events
GET    /api/v1/events/{id}         # Event details
GET    /api/v1/events/{id}/odds    # Current odds
```

#### Betting
```
POST   /api/v1/bets               # Place bet
GET    /api/v1/bets/{id}          # Bet details
GET    /api/v1/bets/user/{addr}   # User's bets
```

#### Oracle
```
GET    /api/v1/oracles            # Oracle status
POST   /api/v1/oracles/submit     # Submit result (oracles only)
GET    /api/v1/consensus/{event}  # Consensus status
```

### WebSocket Events

```javascript
// Real-time updates
const ws = new WebSocket('wss://api.wagerr.com/ws');

ws.on('event_result', (data) => {
  console.log('Event settled:', data);
});

ws.on('bet_placed', (data) => {
  console.log('New bet:', data);
});

ws.on('odds_update', (data) => {
  console.log('Odds changed:', data);
});
```

## Development

### Project Structure

```
wagerr-platform/
├── contracts/              # Clarity smart contracts
│   ├── betting-core.clar
│   ├── token-management.clar
│   └── oracle-system.clar
├── frontend/               # React-based web app
│   ├── src/
│   ├── public/
│   └── package.json
├── backend/                # Node.js API server
│   ├── routes/
│   ├── models/
│   └── services/
├── mobile/                 # React Native app
│   ├── android/
│   ├── ios/
│   └── src/
├── oracle/                 # Oracle masternode software
│   ├── src/
│   ├── config/
│   └── scripts/
├── docs/                   # Documentation
└── tests/                  # Test suites
```

### Testing

```bash
# Run smart contract tests
clarinet test

# Run frontend tests
cd frontend && npm test

# Run backend tests
cd backend && npm test

# Integration tests
npm run test:integration

# End-to-end tests
npm run test:e2e
```

### Code Quality

```bash
# Linting
npm run lint

# Format code
npm run format

# Security audit
npm audit

# Contract analysis
clarinet analyze
```

## Security

### Smart Contract Security

- **Audits**: Regular third-party security audits
- **Formal Verification**: Mathematical proof of contract correctness
- **Bug Bounty**: Ongoing bug bounty program
- **Multi-sig**: Critical functions require multiple signatures

### Oracle Security

- **Decentralization**: Multiple independent oracles
- **Consensus Mechanism**: Super-majority voting (67%+)
- **Reputation System**: Performance-based oracle scoring
- **Slashing**: Penalties for malicious behavior

### User Security

- **Non-custodial**: Users maintain control of private keys
- **No KYC**: Privacy-preserving design
- **Transparent**: All transactions publicly verifiable
- **Immutable**: Results cannot be altered post-settlement

## Roadmap

### Phase 1: Foundation (Q2 2025) ✅
- [x] Core smart contracts deployment
- [x] Basic betting functionality
- [x] Oracle system launch
- [x] Web application release

### Phase 2: Expansion (Q3 2025) 🚧
- [ ] Mobile app release
- [ ] Additional sports coverage
- [ ] Enhanced oracle decentralization
- [ ] Affiliate system on-chain migration

### Phase 3: Growth (Q4 2025) 📋
- [ ] Esports betting integration
- [ ] Advanced betting types
- [ ] Cross-chain compatibility
- [ ] Institutional partnerships

### Phase 4: Maturity (Q1 2026) 📋
- [ ] Regulatory compliance framework
- [ ] Advanced analytics dashboard
- [ ] Community governance implementation
- [ ] Global market expansion

## Contributing

We welcome contributions from the community! Please read our [Contributing Guidelines](CONTRIBUTING.md) before submitting pull requests.

### Development Workflow

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Write tests for your changes
4. Implement your feature
5. Run the test suite (`npm test`)
6. Commit your changes (`git commit -m 'Add amazing feature'`)
7. Push to your branch (`git push origin feature/amazing-feature`)
8. Open a Pull Request

### Contribution Areas

- 🔧 Smart contract development
- 🎨 Frontend/UI improvements
- 📱 Mobile app features
- 🔍 Oracle system enhancements
- 📚 Documentation
- 🧪 Testing and QA
- 🛡️ Security auditing

## Community

- **Discord**: [discord.gg/wagerr](https://discord.gg/wagerr)
- **Telegram**: [@WagerrOfficial](https://t.me/WagerrOfficial)
- **Twitter**: [@wagerr](https://twitter.com/wagerr)
- **Reddit**: [r/Wagerr](https://reddit.com/r/Wagerr)
- **Medium**: [wagerr.medium.com](https://wagerr.medium.com)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

For technical support or questions:

- 📧 Email: [support@wagerr.com](mailto:support@wagerr.com)
- 💬 Discord: [discord.gg/wagerr](https://discord.gg/wagerr)
- 📖 Documentation: [docs.wagerr.com](https://docs.wagerr.com)
- 🐛 Bug Reports: [GitHub Issues](https://github.com/wagerr/wagerr-platform/issues)

---

**Disclaimer**: Betting involves risk. Please bet responsibly and within your means. Wagerr does not provide financial advice. Users are responsible for compliance with local laws and regulations.
