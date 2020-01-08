(use-modules (srfi srfi-64)
	     (mlg port))

(test-begin "t-mlg-port")

(test-assert "read-line-terminator empty string"
	    (eof-object? (read-line-terminator (open-input-string ""))))

(test-assert "read-line-terminator not at eol"
	    (string-null? (read-line-terminator (open-input-string "hello"))))

(test-equal "read-line-terminator RETURN only"
	    "\r" (read-line-terminator (open-input-string "\r")))

(test-equal "read-line-terminator NEWLINE only"
	    "\n" (read-line-terminator (open-input-string "\n")))

(test-equal "read-line-terminator NEL only"
	    "\x85" (read-line-terminator (open-input-string (string #\x85))))

(test-equal "read-line-terminator VTAB only"
	    "\v" (read-line-terminator (open-input-string "\v")))

(test-equal "read-line-terminator LS only"
	    "\u2028" (read-line-terminator (open-input-string (string #\x2028))))

(test-equal "read-line-terminator PS only"
	    "\u2029" (read-line-terminator (open-input-string (string #\x2029))))

(test-equal "read-line-terminator CR + LF"
	    "\r\n" (read-line-terminator (open-input-string "\r\n")))

(test-equal "read-line-terminator CR + CR"
	    "\r" (read-line-terminator (open-input-string "\r\r")))

(test-equal "read-line-terminator LF + CR"
	    "\n" (read-line-terminator (open-input-string "\n\r")))

(test-equal "read-line-terminator LF + LF"
	    "\n" (read-line-terminator (open-input-string "\n\n")))

(test-end "t-mlg-port")
