export enum NetworkSettingEnums {
  EditNetwork = "Edit Network",
  AddNetwork = "Add Network",
}

export enum NetworkEnums {
  Auto = "auto",
  PreviewTestnet = "preview testnet",
  PreprodTestnet = "preprod testnet",
  Mainnet = "mainnet",
  LegacyTestnet = "legacy testnet",
  Localhost = "localhost",
}

export enum AddNetworkErrorEnums {
  EmptyName = "Network name shouldnot be empty!!",
  EmptyUrl = "Url shouldnot be empty!!",
  DuplicateName = "Network name must be unique!!",
}
