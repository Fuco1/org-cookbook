(defun org-cookbook-test--amount-should-be (amount a u)
  "Compare cookbook AMOUNT against A and U."
  (and (equal (org-cookbook-amount-amount amount) a)
       (equal (org-cookbook-amount-unit amount) u)))

(ert-deftest org-cookbook--parse-amount-test ()
  (should (org-cookbook-test--amount-should-be
           (org-cookbook--parse-amount "100g") 100 "g"))
  (should (org-cookbook-test--amount-should-be
           (org-cookbook--parse-amount "100 g") 100 "g"))

  (should (org-cookbook-test--amount-should-be
           (org-cookbook--parse-amount "1g") 1 "g"))
  (should (org-cookbook-test--amount-should-be
           (org-cookbook--parse-amount "1 g") 1 "g"))

  (should (org-cookbook-test--amount-should-be
           (org-cookbook--parse-amount "1.1 g") 1.1 "g"))
  (should (org-cookbook-test--amount-should-be
           (org-cookbook--parse-amount "1.1 g") 1.1 "g"))

  (should (org-cookbook-test--amount-should-be
           (org-cookbook--parse-amount "10,10 kg") 10.1 "kg"))
  (should (org-cookbook-test--amount-should-be
           (org-cookbook--parse-amount "10,20 kg") 10.2 "kg")))
