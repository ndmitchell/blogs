# Continuations and Exceptions

I moved Shake to continuations. There are a few things I learnt, most about continuations and exceptions.

### Continuations with exceptions

You can have exception handling using the IORef to store the handler. It seems to work quite nicely.

### You cannot rely on it for cleanup

For that I have a Cleaner.
