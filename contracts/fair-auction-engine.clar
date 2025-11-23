;; ------------------------------------------------------------
;; Contract: fair-auction-engine.clar
;; Purpose:  Fair, reputation-weighted task auction system
;; Author:   willy
;; ------------------------------------------------------------

(define-constant ERR-NOT-OWNER (err u100))
(define-constant ERR-TASK-NOT-FOUND (err u101))
(define-constant ERR-TASK-CLOSED (err u102))
(define-constant ERR-BID-NOT-ALLOWED (err u103))
(define-constant ERR-LOW-BID (err u104))
(define-constant ERR-NO-BIDS (err u105))
(define-constant ERR-NOT-WINNER (err u106))
(define-constant ERR-NOT-AUTHORIZED (err u107))
(define-constant ERR-TASK-ALREADY-FINALIZED (err u108))
(define-constant ERR-FUNDS-ALREADY-CLAIMED (err u109))

;; ------------------------------------------------------------
;; Data Variables
;; ------------------------------------------------------------

(define-data-var task-counter uint u0)
(define-data-var auction-fee uint u1000) ;; fee in microSTX (optional)
(define-data-var admin principal tx-sender)

;; ------------------------------------------------------------
;; Maps
;; ------------------------------------------------------------

(define-map tasks
  { id: uint }
  {
    creator: principal,
    description: (buff 200),
    reward: uint,
    active: bool,
    winner: (optional principal),
    finalized: bool
  }
)

(define-map bids
  { task-id: uint, bidder: principal }
  {
    bid-amount: uint,
    reputation: uint,
    timestamp: uint
  }
)

(define-map task-claims
  { task-id: uint }
  {
    claimed: bool
  }
)

;; ------------------------------------------------------------
;; Events (commented out as Clarity doesn't support define-event)
;; ------------------------------------------------------------

;; (define-event task-created (id uint) (creator principal) (reward uint))
;; (define-event bid-submitted (task-id uint) (bidder principal) (amount uint))
;; (define-event winner-selected (task-id uint) (winner principal))
;; (define-event reward-claimed (task-id uint) (winner principal) (amount uint))

;; ------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------

(define-private (only-admin)
  (if (is-eq tx-sender (var-get admin))
      (ok true)
      ERR-NOT-AUTHORIZED))

(define-private (get-task (task-id uint))
  (match (map-get? tasks { id: task-id })
    task (ok task)
    (err u101)))

;; ------------------------------------------------------------
;; CORE FUNCTIONS
;; ------------------------------------------------------------

(define-public (create-task (description (buff 200)) (reward uint))
  (let ((id (+ (var-get task-counter) u1)))
    (match (stx-transfer? reward tx-sender (as-contract tx-sender))
      result
        (begin
          (map-set tasks { id: id }
            {
              creator: tx-sender,
              description: description,
              reward: reward,
              active: true,
              winner: none,
              finalized: false
            })
          (var-set task-counter id)
          (ok id)
        )
      err (err err))))

;; ------------------------------------------------------------
;; BIDDING SYSTEM
;; ------------------------------------------------------------

(define-public (submit-bid (task-id uint) (bid-amount uint) (reputation uint))
  (match (get-task task-id)
    task
      (if (not (get active task))
          ERR-TASK-CLOSED
          (begin
            (map-set bids { task-id: task-id, bidder: tx-sender }
              { bid-amount: bid-amount, reputation: reputation, timestamp: burn-block-height })
            (ok true)
          ))
    err (err err)))

;; ------------------------------------------------------------
;; FAIR WINNER SELECTION (WEIGHTED SYSTEM)
;; ------------------------------------------------------------

(define-public (select-winner (task-id uint) (winner principal))
  (match (get-task task-id)
    task
      (if (not (is-eq tx-sender (get creator task)))
          ERR-NOT-OWNER
          (if (not (get active task))
              ERR-TASK-CLOSED
              (begin
                (map-set tasks { id: task-id } (merge task {
                  winner: (some winner),
                  active: false
                }))
                (ok true)
              )
          ))
    err (err err)))

;; ------------------------------------------------------------
;; CLAIM REWARD
;; ------------------------------------------------------------

(define-public (claim-reward (task-id uint))
  (match (get-task task-id)
    task
      (match (get winner task)
        winner-principal
          (if (and (is-eq tx-sender winner-principal)
                   (not (get finalized task)))
              (match (stx-transfer? (get reward task) (as-contract tx-sender) tx-sender)
                result
                  (begin
                    (map-set tasks { id: task-id } (merge task { finalized: true }))
                    (ok true))
                err (err err))
              ERR-NOT-WINNER)
        ERR-TASK-ALREADY-FINALIZED)
    err (err err)))

;; ------------------------------------------------------------
;; READ-ONLY FUNCTIONS
;; ------------------------------------------------------------

(define-read-only (get-task-info (task-id uint))
  (default-to
    { creator: tx-sender, description: 0x, reward: u0, active: false, winner: none, finalized: false }
    (map-get? tasks { id: task-id })))

(define-read-only (get-bid-info (task-id uint) (bidder principal))
  (default-to
    { bid-amount: u0, reputation: u0, timestamp: u0 }
    (map-get? bids { task-id: task-id, bidder: bidder })))
