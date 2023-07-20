module ProductUnfold : module type of Unfold.Make (Product_utils.StringPTNetProduct)
module RepeatedExecSS : ProductUnfold.SearchScheme
include module type of ProductUnfold.Tester (RepeatedExecSS)
