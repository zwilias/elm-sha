# 1.0.5

### Upgraded to 0.18

# 1.0.4

### Test

  * Added `NIST` test cases.

### Code

  * Added support for `HEX` (`0x`) in order to test `NIST`.

# 1.0.3

### Bug

  * `helperPrePenultimate` and `helperPreLast` called for each chunk instead of only last.

# 1.0.2

### Code

  * `hashComputationHelper` was redundant

# 1.0.1

### Documentation

  * Takes a string. Produces a sha1sum (string)

# 1.0.0

### Initial release

  * Added the following functions to `SHA`:

	sha1sum : String -> String
	
	sha224sum : String -> String
	
	sha256sum : String -> String
