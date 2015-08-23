600.426 - Programming Languages
JHU Spring 2015
Homework 4 Part 2 (60 points)

--------------------------------------------------------------------------------------------------
HEADER: PLEASE FILL THIS IN
--------------------------------------------------------------------------------------------------

Name                  : Zhihao Cao
List of team Members  : Hefeng Sun, Lingjie Sang
List of discussants   : Jian Jin

--------------------------------------------------------------------------------------------------
  Section 1: Operational Equivalence
--------------------------------------------------------------------------------------------------

1a. We have considered a number of extensions to Fb in the class. These extensions can sometimes have
    surprising effects on the semantics of the original language. It is quite possible that certain
    extensions cause operationally equivalent expressions from the original language to become
    inequivalent in the extended one. 
   
    For each of the following pairs of languages you have to:
     - Determine if operationally equivalent expressions in the first language remain operationally
       equivalent in the second? [Just answer YES or NO]

     - If you answered NO to the above, write down a pair of expressions that is operationally
       equivalent in the first language, but not in the second. Also provide a context in the second
       language that can tell them apart.
      
       Ensure that the pair of expressions are concrete; they should not use meta-variables. Further
       both expressions must be syntactically valid in *both* languages (Otherwise it does not make
       sense to compare them). For example, when considering Fb and FbS, x + y and y + x are valid
       candidates, but x := y and y := x are not (since these two are not valid in Fb). Of course,
       the contexts are specific to each language.

    1. Fb and FbS
    No.
    In Fb, (f 1) =~ (Function n -> (f 1)) (f 1) 
    But in FbS, (f 1) =/~ (Function n -> (f 1)) (f 1)
    Let context C = (Let x = (Ref 0) In Let f = (Fun y -> y + ((Fun n1 -> !x) (x := !x + 1))  ) In * )
    C[(f 1)] = (Let x = (Ref 0) In Let f = (Fun y -> y + ((Fun n1 -> !x) (x := !x + 1))  ) In (f 1) ) = 2
    C[(Function n -> (f 1)) (f 1)] = (Let x = (Ref 0) In Let f = (Fun y -> y + ((Fun n1 -> !x) (x := !x + 1))  ) In ( (Function n -> (f 1)) (f 1) ) ) = 3
  
     

    2. Fb and FbR
    Yes

    3. Fb and FbX
    No.
    In Fb, (f 1) + (f 2) =~ (f 2) + (f 1)
    But in FbX, (f 1) + (f 2) =/~ (f 2) + (f 1)
    Let context C = (Function f -> *) (Function x -> If x = 1 Then Raise (#Error 1) Else Raise (#Error 2))
    C[(f 1) + (f 2)] = (Function f -> ((f 1) + (f 2))) (Function x -> If x = 1 Then Raise (#Error 1) Else Raise (#Error 2)) = Raise (#Error 1)
    C[(f 2) + (f 1)] = (Function f -> ((f 2) + (f 1))) (Function x -> If x = 1 Then Raise (#Error 1) Else Raise (#Error 2)) = Raise (#Error 2)


    4. FbS and FbX
    Yes




    [12 Points]

1b. Is it possible to produce a context C for FbR such that C[e] returns True when e is a record that has
    a field named 'b' and False if e is a record that does not contain the field 'b'? 
    
    If you think the answer is yes, then write down the context. If not, create an extension to FbR (with
    your own syntax and operational semantics rules) to accomplish this and then write down a
    context that works in this extended language.

    [6 Points]

    No.

    e :: = ... | contains e e


e => {l1 = v1, ..., li = vi, ...}  e2 => vi  
--------------------------------------------
          contains e1 e2 => True

e => {l1 = v1, ..., ln = vn}  e2 => vi  vi is no equal to v1...vn 
------------------------------------------------------------------
                    contains e1 e2 => False


Context:
C = contains * b





--------------------------------------------------------------------------------------------------
  Section 2: Affairs of State
--------------------------------------------------------------------------------------------------

2a. With the introduction of state in FbS, it becomes possible to add traditional imperative 
    constructs like while-loops and for-loops. 
   
    For this question consider an extension for FbS, FbSW which adds a python-style while loop
    expression to FbS with the following grammar: While e: e Else: e. 

    The behavior of an expression While e1: e2 Else: e3 is as follows:
      - As long as e1 evaluates to True, e2 gets evaluated repeatedly (as you would expect in most
        languages)
      - When e1 evaluates to False, e3 gets evaluated (once) instead.
      - The result of the while expression is the result of e3. The result of e2 is never really
        used. (This is different from Python since while in python does not return a value.  But we
        like our values)

    E.g. Let t = Ref 0 in
         Let i = Ref 0 in
         While (Not (!i = 10)):
           t := !t + !i + 1 ;
           i := !i + 1
         Else:
           !t

        returns 55;

    Write down the additional operational semantics rules (beyond those already in FbS) required to
    make FbSW work. Remember that you must write down the *direct* operational semantics for the
    language and not rules based off some encoding.

    [7 Points]



<e1, S1> => <False, S2>  <e3, S2> => <v, S3>  
--------------------------------------------
   <While e1: e2 Else: e3, S1> => <v, S3>


<e1, S1> => <True, S2>  <e2, S2> => <v1, S3>  <While e1: e2 Else: e3, S3> => <v2, S4>
-------------------------------------------------------------------------------------
                      <While e1: e2 Else: e3, S1> => <v2, S4>




2b. For the following programs, write out a proof tree of their execution.

    1. The FbS expression:
       (Function x -> Function y -> y := !y + x) 5 (Ref 0) 

    2. The FbX expression:
       (Function x -> Try If x = 0 Then Raise #ZeroError 0 Else x + 1 With #ZeroError n -> n - 1) 0    

    [10 Points]


                                                                                                                                                                                                                  <c, {c|-> 0}> => <c, {c|-> 0}>
                                                                                                                                                                                                                   --------------------------------    
                                                                                                                                                                                                                    <!c, {c|-> 0}> => <0, {c|-> 0}>  <5,{c|->0}> => <5,{c|->0}>
                                                                                                                                                                                                                   -----------------------------------
                                                                                                                                                                                     <c, {c|-> 0}> => <c, {c|-> 0}>  <!c + 5, {c|-> 0}> => <5,{c|->0}>
                                                                                                                                                                                     --------------------------------------------------------------
<Function x -> Function y -> y := !y + x, {}> => *  <5, {}> => <5, {}>  <(Function y -> y := !y + x)[5/x], {}> => <Function y -> y := !y + 5, {}>          <0, {}> => <0, {}>         <(c := !c + 5), {c|-> 0}> => <5,{c|->0, c|->5}>
-------------------------------------------------------------------------------------------------------------------------------------------------   -----------------------------   ---------------------------------------------------------------
                         <(Function x -> Function y -> y := !y + x) 5, {}> => <Function y -> y := !y + 5, {}>                                        <Ref 0, {}> => <c, {c|-> 0}>   <(y := !y + 5)[c/y], {c|-> 0}> => => <5,{c|->0, c|->5}>
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                                     <(Function x -> Function y -> y := !y + x) 5 (Ref 0), {}> => => <5,{c|->0, c|->5}>




                                                                                                                                   0 => 0
                                                                                                                          -------------------------------------------
                                                                                                           0 = 0 => True  Raise #ZeroError 0 => Raise (#ZeroError 0)
                                                                                                           ---------------------------------------------------------------------
                                                                                                            If 0 = 0 Then Raise #ZeroError 0 Else 0 + 1 => Raise (#ZeroError 0)     (n - 1)[0/n] => -1
                                                                                                           -----------------------------------------------------------------------------------------------------
                                                                                                            Try If 0 = 0 Then Raise #ZeroError 0 Else 0 + 1 With #ZeroError n -> n - 1 => -1
                                                                                                           -----------------------------------------------------------------------------------------
 (Function x -> Try If x = 0 Then Raise #ZeroError 0 Else x + 1 With #ZeroError n -> n - 1) => *   0 => 0  (Try If x = 0 Then Raise #ZeroError 0 Else x + 1 With #ZeroError n -> n - 1)[0/x] => -1
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                  (Function x -> Try If x = 0 Then Raise #ZeroError 0 Else x + 1 With #ZeroError n -> n - 1) 0 => -1




--------------------------------------------------------------------------------------------------
  Section 3: Objects and Encoding
----------------------------------------------------------------------------------------

3a. Write the FbOb code corresponding to the following simple (and quite cheesy) class
    hierarchy written out in Java:

    public class CreditCard
    {
      private int maxCredit = 0, usedCredit = 0 ;

      public int getMaxCredit() { return maxCredit; }
      public int setMaxCredit(int max) { maxCredit = max ; return maxCredit; }
      public int getAvailableCredit() { return maxCredit - usedCredit; }
      public int swipe(int money) { usedCredit += money ; return getAvailableCredit(); }
      public int pay(int money) { usedCredit -= money ; return getAvailableCredit() ; } 
    }

    public class CashbackCard extends CreditCard
    {
        private int payback = 10 ;

        public int getPayback() { return payback; }
        public int setPayback(int p) { payback = p; return payback; }

        @Override
        public int pay(int money) {
          swipe(money) ;
          if (money > 100)
            pay(payback);
          return getAvailableCredit() ;
        }
    }

    The FbOb code should mirror the features and behavior of the Java code.

    [10 Points]


Let creditCardClass =
  Class Extends EmptyClass
    Inst
      maxCredit = 0;
      usedCredit = 0
    Meth
      getMaxCredit = Function _ -> maxCredit;
      setMaxCredit = Function max -> maxCredit := max;
      getAvailableCredit = Function _ -> maxCredit - usedCredit;
      swipe = Function money -> usedCredit := usedCredit + money; This <- getAvailableCredit {};
      pay = Function money -> usedCredit := usedCredit - money; This <- getAvailableCredit {}

In Let cashbackCardClass =
  Class Extends creditCardClass
    Inst
      maxCredit = 0;
      usedCredit = 0;
      payback = 10
    Meth
      getMaxCredit = Super <- getMaxCredit;
      setMaxCredit = Super <- setMaxCredit;
      getAvailableCredit = Super <- getAvailableCredit;
      swipe = Super <- swipe;
      getPayback = Function _ -> payback;
      setPayback = Function p -> payback := p;
      pay = Function money -> This <- swipe money; 
              Let less_than = Function e1 -> Function e2 -> e1 = e2 In 
                If (less_than 100 money) Then super <- pay payback Else 1;
                  this <- getAvailableCredit {}

In Let creditCard = New creditCardClass
In Let CashbackCard = New cashbackCardClass In
  CashbackCard <- getMaxCredit {}



3b. Provide an FbSR translation for the FbOb code that you produced for 3a. For
    reference, the translation rules and examples are in section 5.2.3 of the book.

    You can test your implementation using the FbSR binaries provided on the course
    website.

    [15 Points]

(*dummy less_than function*)
Let less_than = Function e1 -> Function e2 -> e1 = e2 In

Let creditCardClass =
  Function _ -> Let super = (Function _ -> {}) {} In {
    inst = {
      maxCredit = Ref 0;
      usedCredit = Ref 0
    };
    meth = {
      getMaxCredit = Function this -> Function _ -> !(this.inst.maxCredit);
      setMaxCredit = Function this -> Function max ->  (this.inst.maxCredit) := max;
      getAvailableCredit = Function this -> Function _ -> (!(this.inst.maxCredit)) - (!(this.inst.usedCredit));
      swipe = Function this -> Function money -> 
        ( (Function n -> ((this.meth.getAvailableCredit) this {})) ((this.inst.usedCredit) := (!(this.inst.usedCredit)) + money) );
      pay = Function this -> Function money -> 
        ( (Function n -> ((this.meth.getAvailableCredit) this {})) ( (this.inst.usedCredit) := (!(this.inst.usedCredit)) - money ) )
    }
  }

In Let cashbackCardClass =
  Function _ -> Let super = creditCardClass {} In {
    inst = {
      maxCredit = Ref 0;
      usedCredit = Ref 0;
      payback = Ref 10
    };
    meth = {
      getMaxCredit = Function this -> Function _ -> (super.meth.getMaxCredit) this {};
      setMaxCredit = Function this -> Function max -> (super.meth.setMaxCredit) this max;
      getAvailableCredit = Function this -> Function _ -> (super.meth.getAvailableCredit) this {};
      swipe = Function this -> Function money -> (super.meth.swipe) this money;
      getPayback = Function this -> Function _ -> !(this.inst.payback);
      setPayback = Function this -> Function p -> (this.inst.payback) := p;
      pay = Function this -> Function money -> ((Function n1 -> Function n2 -> ( (this.meth.getAvailableCredit) this {} )) 
        ((this.meth.swipe) this money) (If (less_than 100 money) Then ((super.meth.pay) this (this.inst.payback)) Else 1 ) )
    }
  }

In 
  Let cashbackCard = cashbackCardClass {} In
    (cashbackCard.meth.getMaxCredit) cashbackCard {};;







