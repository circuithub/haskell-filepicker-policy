# make sure cabal is configured to run tests
cabal configure --enable-tests
# run all the test showing their output and colorized
cabal test --show-details=always --test-options="--color"
