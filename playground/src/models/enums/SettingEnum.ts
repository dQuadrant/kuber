export enum NetworkSettingEnums {
  EditNetwork = "Edit Network",
  AddNetwork = "Add Network",
}

export enum NetworkEnums {
  Auto = "Auto",
  PreviewTestnet = "Preview Testnet",
  PreprodTestnet = "Preprod Testnet",
  Mainnet = "Mainnet",
  LegacyTestnet = "Legacy Testnet",
  Localhost = "Localhost",
}

export enum AddNetworkErrorEnums {
  EmptyName = "Network name shouldnot be empty!!",
  EmptyUrl = "Url shouldnot be empty!!",
  DuplicateName = "Network name must be unique!!",
}
