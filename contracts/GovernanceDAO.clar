;; GovernanceDAO - Decentralized Voting & Proposal Management
;; Transparent governance with stake-weighted voting

;; Data storage
(define-map dao-members principal {
  active: bool,
  voting-power: uint,
  staked-amount: uint,
  proposals-created: uint,
  votes-cast: uint,
  joined-at: uint
})

(define-map governance-proposals uint {
  proposer: principal,
  title: (string-ascii 128),
  description: (string-ascii 512),
  proposal-type: (string-ascii 32),
  votes-for: uint,
  votes-against: uint,
  status: (string-ascii 32),
  created-at: uint,
  voting-deadline: uint,
  execution-date: uint
})

(define-map member-votes {proposal-id: uint, member: principal} {
  vote-choice: (string-ascii 16),
  voting-power-used: uint,
  timestamp: uint
})

(define-map treasury-records uint {
  transaction-type: (string-ascii 32),
  amount: uint,
  recipient: principal,
  timestamp: uint,
  status: (string-ascii 32)
})

;; Constants
(define-constant ERR_NOT_AUTHORIZED (err u600))
(define-constant ERR_INVALID_PARAMS (err u601))
(define-constant ERR_MEMBER_NOT_FOUND (err u602))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u603))
(define-constant ERR_ALREADY_REGISTERED (err u604))
(define-constant ERR_INVALID_PRINCIPAL (err u605))
(define-constant ERR_INSUFFICIENT_VOTING_POWER (err u606))
(define-constant ERR_ALREADY_VOTED (err u607))
(define-constant ERR_VOTING_CLOSED (err u608))
(define-constant ERR_INSUFFICIENT_STAKE (err u609))

(define-constant ZERO_ADDRESS 'SP000000000000000000002Q6VF78)
(define-constant MIN_STAKE_AMOUNT u10000)
(define-constant VOTING_PERIOD u604800)
(define-constant MIN_PROPOSAL_TITLE_LENGTH u10)
(define-constant MAX_PROPOSAL_DESCRIPTION_LENGTH u512)

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var next-proposal-id uint u1)
(define-data-var next-treasury-id uint u1)
(define-data-var total-staked uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var governance-fee-percent uint u2)

;; Admin functions
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq new-owner ZERO_ADDRESS)) ERR_INVALID_PRINCIPAL)
    (ok (var-set contract-owner new-owner))))

(define-public (set-governance-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u10) ERR_INVALID_PARAMS)
    (ok (var-set governance-fee-percent new-fee))))

;; Member functions
(define-public (join-dao (stake-amount uint))
  (begin
    (asserts! (is-none (map-get? dao-members tx-sender)) ERR_ALREADY_REGISTERED)
    (asserts! (>= stake-amount MIN_STAKE_AMOUNT) ERR_INSUFFICIENT_STAKE)
    
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set dao-members tx-sender {
      active: true,
      voting-power: stake-amount,
      staked-amount: stake-amount,
      proposals-created: u0,
      votes-cast: u0,
      joined-at: u0
    })
    
    (var-set total-staked (+ (var-get total-staked) stake-amount))
    
    (ok stake-amount)))

(define-public (increase-stake (additional-stake uint))
  (let ((member (unwrap! (map-get? dao-members tx-sender) ERR_MEMBER_NOT_FOUND)))
    (asserts! (> additional-stake u0) ERR_INVALID_PARAMS)
    
    (try! (stx-transfer? additional-stake tx-sender (as-contract tx-sender)))
    
    (let ((new-voting-power (+ (get voting-power member) additional-stake)))
      (map-set dao-members tx-sender (merge member {
        voting-power: new-voting-power,
        staked-amount: (+ (get staked-amount member) additional-stake)
      }))
      
      (var-set total-staked (+ (var-get total-staked) additional-stake))
      
      (ok new-voting-power))))

(define-public (leave-dao)
  (let ((member (unwrap! (map-get? dao-members tx-sender) ERR_MEMBER_NOT_FOUND)))
    (asserts! (get active member) ERR_MEMBER_NOT_FOUND)
    
    (let ((staked-amount (get staked-amount member)))
      (try! (as-contract (stx-transfer? staked-amount tx-sender tx-sender)))
      
      (map-set dao-members tx-sender (merge member {active: false, voting-power: u0}))
      
      (var-set total-staked (- (var-get total-staked) staked-amount))
      
      (ok staked-amount))))

;; Proposal functions
(define-public (create-proposal (title (string-ascii 128)) (description (string-ascii 512)) (proposal-type (string-ascii 32)))
  (let (
    (member (unwrap! (map-get? dao-members tx-sender) ERR_MEMBER_NOT_FOUND))
    (proposal-id (var-get next-proposal-id))
  )
    (asserts! (get active member) ERR_MEMBER_NOT_FOUND)
    (asserts! (>= (len title) MIN_PROPOSAL_TITLE_LENGTH) ERR_INVALID_PARAMS)
    (asserts! (> (len description) u0) ERR_INVALID_PARAMS)
    (asserts! (> (len proposal-type) u0) ERR_INVALID_PARAMS)
    
    (map-set governance-proposals proposal-id {
      proposer: tx-sender,
      title: title,
      description: description,
      proposal-type: proposal-type,
      votes-for: u0,
      votes-against: u0,
      status: "active",
      created-at: u0,
      voting-deadline: (+ u0 VOTING_PERIOD),
      execution-date: u0
    })
    
    (map-set dao-members tx-sender (merge member {
      proposals-created: (+ (get proposals-created member) u1)
    }))
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (vote-choice (string-ascii 16)))
  (let (
    (member (unwrap! (map-get? dao-members tx-sender) ERR_MEMBER_NOT_FOUND))
    (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (vote-key {proposal-id: proposal-id, member: tx-sender})
  )
    (asserts! (get active member) ERR_MEMBER_NOT_FOUND)
    (asserts! (is-eq (get status proposal) "active") ERR_VOTING_CLOSED)
    (asserts! (is-none (map-get? member-votes vote-key)) ERR_ALREADY_VOTED)
    (asserts! (or (is-eq vote-choice "for") (is-eq vote-choice "against")) ERR_INVALID_PARAMS)
    
    (let ((voting-power (get voting-power member)))
      (map-set member-votes vote-key {
        vote-choice: vote-choice,
        voting-power-used: voting-power,
        timestamp: u0
      })
      
      (if (is-eq vote-choice "for")
        (map-set governance-proposals proposal-id (merge proposal {
          votes-for: (+ (get votes-for proposal) voting-power)
        }))
        (map-set governance-proposals proposal-id (merge proposal {
          votes-against: (+ (get votes-against proposal) voting-power)
        }))
      )
      
      (map-set dao-members tx-sender (merge member {
        votes-cast: (+ (get votes-cast member) u1)
      }))
      
      (ok true))))

(define-public (finalize-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? governance-proposals proposal-id) ERR_PROPOSAL_NOT_FOUND)))
    (asserts! (is-eq (get status proposal) "active") ERR_INVALID_PARAMS)
    
    (let ((status-result (if (> (get votes-for proposal) (get votes-against proposal)) "approved" "rejected")))
      (map-set governance-proposals proposal-id (merge proposal {
        status: status-result,
        execution-date: u0
      }))
      
      (ok status-result))))

(define-public (withdraw-governance-fees)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (let ((amount (var-get treasury-balance)))
      (asserts! (> amount u0) ERR_INVALID_PARAMS)
      
      (try! (as-contract (stx-transfer? amount tx-sender (var-get contract-owner))))
      
      (var-set treasury-balance u0)
      
      (ok amount))))

;; Read-only functions
(define-read-only (get-dao-member (member principal))
  (map-get? dao-members member))

(define-read-only (get-governance-proposal (proposal-id uint))
  (map-get? governance-proposals proposal-id))

(define-read-only (get-member-vote (proposal-id uint) (member principal))
  (map-get? member-votes {proposal-id: proposal-id, member: member}))

(define-read-only (get-total-staked)
  (var-get total-staked))

(define-read-only (get-treasury-balance)
  (var-get treasury-balance))

(define-read-only (get-governance-fee)
  (var-get governance-fee-percent))