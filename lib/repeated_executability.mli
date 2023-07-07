module ProductUnfold : module type of Unfold.Make (Product)
module RepeatedExecSS : ProductUnfold.SearchScheme
include module type of ProductUnfold.Tester (RepeatedExecSS)
