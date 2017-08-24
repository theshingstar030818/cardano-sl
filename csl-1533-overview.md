# Block and transaction processing

## Current situation

A node is processing blocks and transactions simultaneously. Transaction
processing depends on the GState (to validate the transaction against it) and
modifies the mempool (adds the transaction to it). Block processing depends on
the mempool (to create a block), modifies the mempool (to normalize it after
adding a block), and modifies the GState (adding a block).

The properties we care about:

* B-PRIORITY: block processing takes priority over transaction processing
* T-NORMALIZATION: block processing uses a normalized mempool (to avoid invalid transactions)

### Transaction processing

The procedure responsible for processing a transaction is
`txProcessTransaction`, defined in `txp/Pos/Txp/Logic/Local.hs`. There's also a
version with support for explorer, `eTxProcessTransaction`, which is apparently
done via copy-paste, so any changes must be reflected there.

`txProcessTransaction` is called in:

* `saveTxDefault` -- used in the wallet web mode;
* `genTxPayload` -- not important, it's a part of the block generator for tests;
* `handleTxDo`, which is used in `txInvReqDataParams`, which is used in
  `txRelays`.

`txRelays` is a listener that accepts incoming transactions, processes them, and
relays to other nodes. It's a single listener, therefore a single thread
(presumably).

`handleTxDo` is a thin wrapper over `txProcessTransaction` that dispatches
between the explorer and the regular version and logs whether the transaction
was processed successfully (added to storage) or not.

`txProcessTransaction` starts by getting the adopted `BlockVersionData` from the
GState, the current epoch (determined by `getCurrentSlot`), genesis
stakeholders, and `UtxoModifier`. Then it resolves the transaction inputs using
GState (`utxoGet` = `getTxOut`, `getStake = getRealStake`).

The collected data and resolved inputs together form a `ProcessTxContext`. Using
this context, we modify `TxpLocalData` (a part of the mempool). Modification of
`TxpLocalData` is done via the `modifyTxpLocalData` helper, which guarantees
atomicity (by an internal `MVar` used as a lock).

The issue is that the data that we were collecting for `ProcessTxContext` might
have been modified by other threads even while we were collecting it, so even
multiple `utxoGet` calls might have been performed against different GState. If
this happens, the transaction we process probably becomes invalid, but it still
might be added to `TxpLocalData`. The trick is to guarantee that no one will use
the invalid transaction before it is deleted by normalization.

This is done by taking the tip in the GState before starting data collection,
and comparing it with the tip in `TxpLocalData` before adding a transaction.
However, it's not obvious why this guarantees correctness (but it does, see the
proof below).

### Block processing

We're concerned with block processing because it can modify `TxpLocalData` as
well, using the `normalizeMempool` and `clearTxpMemPool` procedures, which are
calling `modifyTxpLocalData` (therefore are atomic).

There's also `setTxpLocalData`, but it's only used to implement `txNormalize`
and `eTxNormalize` (both are a part of `normalizeMempool`).

`normalizeMempool` is called in:

* `createGenesisBlockDo` -- used in `createGenesisBlockAndApply` within
  `withBlkSemaphore`;
* `genBlock` -- not important, it's a part of the block generator for tests;
* `verifyAndApplyBlocks` -- used in tests (not important), and in
  `applyWithoutRollback` within `withBlkSemaphore`.

`clearTxpMemPool` is called in `applyCreatedBlock`, which is used in
`createdMainBlockAndApply` within `withBlkSemaphore`.

The thing to notice here is that these modifications to `TxpLocalData` all
take place within `withBlkSemaphore`.

## Semi-formal proof of correctness

* B-PRIORITY. Block processing will take priority over transaction processing
    as long as there's no blocking on transaction processing. There are two locking
    mechanisms to consider: `withBlkSemaphore` and `modifyTxpLocalData`. The former
    is not used in transaction processing; the latter is used in block-processing
    only after the block has been added to GState.

    A remark: although we use `modifyTxpLocalData` after adding the block to GState,
    so processing of each block in isolation will take priority, we do it within
    `withBlkSemaphore`, so we still might wait for transaction processing to finish
    before releasing the lock on block processing. This means that if we need to
    process multiple blocks in a row, transaction processing may delay us.

* T-NORMALIZATION. Block processing will access a normalized mempool as long as
    no action has denormalized it since last normalization.

    axiom B-ONLY: The only source of GState modifications is block processing.

    axiom T-VALIDITY: Transaction processing will yield a valid transaction as
    long as GState was not modified and the mempool is in sync with GState. Adding
    a valid transaction to the mempool preserves T-NORMALIZATION.

    lemma B-N-INTERLEAVED: Block processing and mempool normalization are
    interleaved. This is true because we always perform mempool normalization
    after block processing within `withBlkSemaphore`. Essentially, we have
    the following loop:

    * lock taken
    * block processing
    * mempool normalization
    * lock released

    During transaction processing, there are two major phases. (1) Collecting the data
    for `ProcessTxContext`, and (2) creating the transaction itself (within
    `modifyTxpLocalData`). In order to rely on T-VALIDITY, we must show that
    GState was not modified during (1) and the mempool is in sync with GState
    during (2). When we cannot show that, we must show that the possibly invalid
    transaction will be removed by normalization before block processing takes
    place.

    Assume the GState was not modified. Then we establish that the mempool is in
    sync by comparing the tip at the beginning of transaction processing with the
    tip in the mempool. Therefore, T-NORMALIZATION holds by T-VALIDITY.

    Assume the GState was modified. Then by B-ONLY we have that block processing
    is in progress. However, the transaction we're about to add to the mempool
    might be invalid. By B-N-INTERLEAVED, we have that either the mempool was
    normalized since the GState has changed or will be normalized:

    * if it was normalized, the mempool is not in sync with the GState,
    and T-NORMALIZATION holds because we reject the transaction.

    * if it wasn't normalized, we add the invalid transaction to the mempool.
    T-NORMALIZATION holds because the pending normalization will remove
    the added transaction.

    A remark on exception safety: it appears to me that B-N-INTERLEAVED relies on
    the fact that mempool normalization will happend. On the other hand, if an
    exception occurs during block processing, normalization might not happen. Do we
    need to put it into `bracket`?

## Infelicities and solutions

The issue with the current approach is that we reject transactions too often.

Proposed solutions:

  * Make block processing faster. If block processing is fast enough, we'll do
    it quickly, and there won't be lots of transactions that happen to get
    invalidated because of it. Unfortunately, this is not an actionable
    strategy: it's unclear how to make block processing faster and how to keep
    it fast enough.

  * Retry processing the transaction if it was rejected. The transactions
    are rejected when the mempool gets out of sync with GState, but the
    transaction might be valid nonetheless (for the new GState/mempool). It's
    possible to retry processing them rather throw them right away.

  * Move mempool normalization out of `withBlkSemaphore`. It's not clear to me
    why this is a correct approach, as we can no longer rely on B-N-INTERLEAVED.

  * Use `withBlkSemaphore` to synchronize block processing and transaction
    processing. Done in a naive way, this robs us of the B-PRIORITY property.
    We might add a kostyl that block processing sends an async exception to the
    transaction processing thread, in order to get the lock: this might work,
    but it's to tricky to get right (discussed in a HBP meeting).

  * Use a priority queue instead of `withBlkSemaphore`. This should give us
    the properties we desire and Duncan Coutts proposed a two-prio lock
    implementation.
