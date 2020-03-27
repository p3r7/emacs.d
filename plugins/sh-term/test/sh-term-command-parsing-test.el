

(require 'ert)
(require 'prf-tramp-friendly-parsing)




(ert-deftest sh-term-parse-input-pipe-test ()
  "Ensure command w/ a pipe gets correctly parsed."
  (let ((command "ls | grep one")
        (expected '((("ls" nil) ("grep" ("one"))))))
    (should (eq (sh-term--parse-input command) expected))))


(ert-deftest sh-term-parse-input-concat-test ()
  "Ensure command w/ a pipe gets correctly parsed."
  (let ((command "ls >> file | grep one")
        (expected '((("ls" nil) ("grep" ("one"))))))
    (should (eq (sh-term--parse-input command) expected))))
