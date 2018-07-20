.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Add setField to HasField
========================

This proposal both extends and modifies the existing
`Overloaded Record Fields proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0002-overloaded-record-fields.rst>`.

Additions
---------

We add an additional member ``setField`` to the ``HasField`` class, so the resulting definition becomes:

.. code-block:: haskell

  -- | Constraint representing the fact that the field @x@ belongs to
  -- the record type @r@ and has field type @a@.  This will be solved
  -- automatically, but manual instances may be provided as well.
  class HasField (x :: k) r a | x r -> a where
    -- | Selector function to extract the field from the record.
    getField :: r -> a
    -- | Update function to set a field in the record.
    setField :: a -> r -> r

The ``setField`` method allows us to perform type-preserving updates.

A *type-changing update* is one where the type ``r`` is higher-kinded and the field ``x`` is the only member of that type. As an example, given a value of type ``(Int, Bool)``, the selector pointing to the first component, and a new value of type ``Double`` we can produce ``(Int, Bool)``. The design space for type-changing updates is large, and almost certainly requires an additional type class. In contrast, the design space for type-preserving updates is small and it can easily be incorporated into the existing type class. The addition of type-preserving updates in no way constraints the design space for future type-changing updates, but is useful in its own right.

Deletions
---------

From the original proposal we remove the changes to ``OverloadedLabels`` as these are orthogonal, and inhibits experimentation.

Implementation Plan
-------------------

Adam Gundry has offered to implement this feature.
