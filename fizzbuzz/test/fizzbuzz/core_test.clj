(ns fizzbuzz.core-test
  (:use midje.sweet)
  (:use [fizzbuzz.core]))

(facts "FizzBuzz"
       (fact "By default it should return the number in String"
             (fizzbuzz 1) => "1")

       (fact "it should add Fizz when divisible by 3"
             (fizzbuzz 6) => "Fizz")

       (fact "it should replace 3 by Fizz"
             (fizzbuzz 33) => "FizzFizzFizz")

       (fact "it should add Buzz when divisible by 5"
             (fizzbuzz 10) => "Buzz")

       (fact "it should add Wazz when divisible by 7"
             (fizzbuzz 14) => "Wazz"
             (fizzbuzz 21) => "FizzWazz"))
