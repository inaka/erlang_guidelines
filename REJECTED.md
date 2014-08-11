## Rejected rules and suggestions

***

##### rule
>  Use tail-recursive functions instead of foldl

##### rejected because
  We love high-order functions!

***

##### rule
>  Using ``'andalso'``, ``'orelse'``, and the like, might save a 'case' or two

##### rejected because
  They're not intended to be used that way and the resulting code messes up with dialyzer