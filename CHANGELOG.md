# unreleased

* New tag syntax:
  ```hs
  case x of
    { 1 -> something
    ; 2 -> another
    }
  ```
  is now written as
  ```hs
  case x of
    { <1> -> something
    ; <2> -> another
    }
  ```

