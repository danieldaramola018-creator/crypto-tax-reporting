
;; title: tax-calculator
;; version: 1.0.0
;; summary: Cryptocurrency tax calculation and reporting system
;; description: Smart contract for automated tax calculation of digital asset transactions
;;             with capital gains tracking and transaction history management

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_OWNER (err u100))
(define-constant ERR_INVALID_TRANSACTION (err u101))
(define-constant ERR_TRANSACTION_NOT_FOUND (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_INVALID_DATE (err u104))
(define-constant ERR_USER_NOT_REGISTERED (err u105))
(define-constant FIFO_METHOD u1)
(define-constant LIFO_METHOD u2)
(define-constant AVERAGE_COST_METHOD u3)

;; data vars
(define-data-var next-transaction-id uint u1)
(define-data-var next-user-id uint u1)
(define-data-var total-users uint u0)
(define-data-var total-transactions uint u0)

;; data maps
(define-map users
  { user-address: principal }
  {
    user-id: uint,
    registration-date: uint,
    preferred-method: uint,
    is-active: bool
  }
)

(define-map transactions
  { transaction-id: uint }
  {
    user-address: principal,
    asset-symbol: (string-ascii 10),
    transaction-type: (string-ascii 4), ;; "BUY" or "SELL"
    amount: uint,
    price-per-unit: uint, ;; in micro-units (1 STX = 1,000,000 micro-STX)
    transaction-date: uint,
    exchange-name: (optional (string-ascii 20)),
    is-processed: bool
  }
)

(define-map capital-gains
  { user-address: principal, tax-year: uint }
  {
    total-gains: int,
    total-losses: int,
    net-gains: int,
    transactions-count: uint
  }
)

(define-map asset-holdings
  { user-address: principal, asset-symbol: (string-ascii 10) }
  {
    total-amount: uint,
    average-cost-basis: uint,
    first-purchase-date: uint,
    last-transaction-date: uint
  }
)

;; public functions

;; Register a new user for tax reporting
(define-public (register-user (preferred-method uint))
  (let
    (
      (user-id (var-get next-user-id))
      (current-block-height stacks-block-height)
    )
    (asserts! (or (is-eq preferred-method FIFO_METHOD)
                  (is-eq preferred-method LIFO_METHOD)
                  (is-eq preferred-method AVERAGE_COST_METHOD))
              ERR_INVALID_TRANSACTION)
    (asserts! (is-none (map-get? users { user-address: tx-sender }))
              ERR_INVALID_TRANSACTION)
    
    (map-set users
      { user-address: tx-sender }
      {
        user-id: user-id,
        registration-date: current-block-height,
        preferred-method: preferred-method,
        is-active: true
      }
    )
    
    (var-set next-user-id (+ user-id u1))
    (var-set total-users (+ (var-get total-users) u1))
    
    (ok user-id)
  )
)

;; Record a cryptocurrency transaction
(define-public (record-transaction
  (asset-symbol (string-ascii 10))
  (transaction-type (string-ascii 4))
  (amount uint)
  (price-per-unit uint)
  (transaction-date uint)
  (exchange-name (optional (string-ascii 20)))
  )
  (let
    (
      (transaction-id (var-get next-transaction-id))
      (user-data (unwrap! (map-get? users { user-address: tx-sender }) ERR_USER_NOT_REGISTERED))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> price-per-unit u0) ERR_INVALID_AMOUNT)
    (asserts! (> transaction-date u0) ERR_INVALID_DATE)
    (asserts! (or (is-eq transaction-type "BUY") (is-eq transaction-type "SELL"))
              ERR_INVALID_TRANSACTION)
    
    ;; Record the transaction
    (map-set transactions
      { transaction-id: transaction-id }
      {
        user-address: tx-sender,
        asset-symbol: asset-symbol,
        transaction-type: transaction-type,
        amount: amount,
        price-per-unit: price-per-unit,
        transaction-date: transaction-date,
        exchange-name: exchange-name,
        is-processed: false
      }
    )
    
    ;; Update asset holdings
    (unwrap! (update-asset-holdings asset-symbol transaction-type amount price-per-unit transaction-date) ERR_INVALID_TRANSACTION)
    
    (var-set next-transaction-id (+ transaction-id u1))
    (var-set total-transactions (+ (var-get total-transactions) u1))
    
    (ok transaction-id)
  )
)

;; Calculate capital gains for a specific tax year
(define-public (calculate-capital-gains (tax-year uint))
  (let
    (
      (user-data (unwrap! (map-get? users { user-address: tx-sender }) ERR_USER_NOT_REGISTERED))
    )
    (asserts! (> tax-year u2020) ERR_INVALID_DATE) ;; Reasonable tax year validation
    (asserts! (< tax-year u2100) ERR_INVALID_DATE)
    
    ;; This is a simplified calculation - in a full implementation,
    ;; this would iterate through all transactions for the year
    (let
      (
        (total-gains 0)
        (total-losses 0)
        (net-gains 0)
      )
      
      (map-set capital-gains
        { user-address: tx-sender, tax-year: tax-year }
        {
          total-gains: total-gains,
          total-losses: total-losses,
          net-gains: net-gains,
          transactions-count: u0
        }
      )
      
      (ok net-gains)
    )
  )
)

;; Process pending transactions for tax calculations
(define-public (process-transaction (transaction-id uint))
  (let
    (
      (tx-data (unwrap! (map-get? transactions { transaction-id: transaction-id }) ERR_TRANSACTION_NOT_FOUND))
    )
    (asserts! (is-eq (get user-address tx-data) tx-sender) ERR_NOT_OWNER)
    (asserts! (not (get is-processed tx-data)) ERR_INVALID_TRANSACTION)
    
    ;; Mark transaction as processed
    (map-set transactions
      { transaction-id: transaction-id }
      (merge tx-data { is-processed: true })
    )
    
    (ok true)
  )
)

;; read-only functions

;; Get user registration information
(define-read-only (get-user-info (user-address principal))
  (map-get? users { user-address: user-address })
)

;; Get transaction details
(define-read-only (get-transaction (transaction-id uint))
  (map-get? transactions { transaction-id: transaction-id })
)

;; Get capital gains for a specific tax year
(define-read-only (get-capital-gains (user-address principal) (tax-year uint))
  (map-get? capital-gains { user-address: user-address, tax-year: tax-year })
)

;; Get asset holdings for a user
(define-read-only (get-asset-holdings (user-address principal) (asset-symbol (string-ascii 10)))
  (map-get? asset-holdings { user-address: user-address, asset-symbol: asset-symbol })
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-users: (var-get total-users),
    total-transactions: (var-get total-transactions),
    next-transaction-id: (var-get next-transaction-id),
    next-user-id: (var-get next-user-id)
  }
)

;; Check if user is registered
(define-read-only (is-user-registered (user-address principal))
  (is-some (map-get? users { user-address: user-address }))
)

;; private functions

;; Update asset holdings based on transaction
(define-private (update-asset-holdings
  (asset-symbol (string-ascii 10))
  (transaction-type (string-ascii 4))
  (amount uint)
  (price-per-unit uint)
  (transaction-date uint)
  )
  (let
    (
      (current-holdings (default-to
        {
          total-amount: u0,
          average-cost-basis: u0,
          first-purchase-date: transaction-date,
          last-transaction-date: transaction-date
        }
        (map-get? asset-holdings { user-address: tx-sender, asset-symbol: asset-symbol })
      ))
    )
    (if (is-eq transaction-type "BUY")
      ;; Handle BUY transaction - add to holdings
      (let
        (
          (new-total-amount (+ (get total-amount current-holdings) amount))
          (total-cost (+ (* (get total-amount current-holdings) (get average-cost-basis current-holdings))
                        (* amount price-per-unit)))
          (new-average-cost (if (> new-total-amount u0) (/ total-cost new-total-amount) u0))
        )
        (map-set asset-holdings
          { user-address: tx-sender, asset-symbol: asset-symbol }
          {
            total-amount: new-total-amount,
            average-cost-basis: new-average-cost,
            first-purchase-date: (if (is-eq (get total-amount current-holdings) u0)
                                   transaction-date
                                   (get first-purchase-date current-holdings)),
            last-transaction-date: transaction-date
          }
        )
      )
      ;; Handle SELL transaction - subtract from holdings
      (let
        (
          (new-total-amount (if (>= (get total-amount current-holdings) amount)
                              (- (get total-amount current-holdings) amount)
                              u0))
        )
        (map-set asset-holdings
          { user-address: tx-sender, asset-symbol: asset-symbol }
          {
            total-amount: new-total-amount,
            average-cost-basis: (get average-cost-basis current-holdings),
            first-purchase-date: (get first-purchase-date current-holdings),
            last-transaction-date: transaction-date
          }
        )
      )
    )
    
    (ok true)
  )
)

