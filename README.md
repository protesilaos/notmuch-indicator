# notmuch-indicator for Emacs

This is a simple package that renders an indicator with an email count
of the `notmuch` index on the Emacs mode line.  The underlying mechanism
is that of `notmuch-count(1)`, which is used to find the number of items
that match the given search terms.

The indicator is enabled when `notmuch-indicator-mode` is on.

The user option `notmuch-indicator-args` provides the means to define
search terms and associate them with a given label.  The label is purely
cosmetic, though it helps characterise the resulting counter.

The value of `notmuch-indicator-args` is a list of plists (property
lists).  Each plist consists of two properties, both of which accept a
string value:

1. The `:terms` holds the command-line arguments passed to
   `notmuch-count(1)` (read the Notmuch documentation for the
   technicalities).

2. The `:label` is an arbitrary string that is prepended to the return
   value of the above.

Multiple plists represent separate `notmuch-count(1)` queries.  These
are run sequentially.  Their return values are joined into a single
string.

For instance, a value like the following defines three commands:

```elisp
(setq notmuch-indicator-args
      '((:terms "tag:unread and tag:inbox" :label "@")
        (:terms "from:authorities and tag:unread" :label "👺")
        (:terms "--output threads tag:loveletter" :label "💕")))
```

These form a string like: `@50 👺1000 💕0`.

The user option `notmuch-indicator-refresh-count` determines how often
the indicator will be refreshed.  It accepts a numeric argument which
represents seconds.

The user option `notmuch-indicator-force-refresh-commands` accepts as
its value a list of symbols.  Those are commands that will forcefully
update the indicator after they are invoked.

The user option `notmuch-indicator-hide-empty-counters` hides zero
counters from the indicator, when it is set to a non-nil value.
