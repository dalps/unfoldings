module ProductUnfold : module type of Unfold.Make (Product_utils.StringPTNetProduct)
module ExecutabilitySS : ProductUnfold.SearchScheme
include module type of ProductUnfold.Tester (ExecutabilitySS)
