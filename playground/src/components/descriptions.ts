import { KuberDiscriptionEnums } from "@/models/enums/DescriptionEnum";

export default class Description {
  static kuberDescription(word: String): String {
    switch (word) {
      case KuberDiscriptionEnums.Selections:
        return selectionsDescription;
      case KuberDiscriptionEnums.Inputs:
        return inputsDescription;
      case KuberDiscriptionEnums.MetaData:
        return metadataDescription;
      case KuberDiscriptionEnums.Mint:
        return mintDescription;
      case KuberDiscriptionEnums.Outputs:
        return outputsDescription;
      default:
        return "No descriptions found";
    }
  }
}

const selectionsDescription =
  "List of utxos/addresses that can be used for balancing transaction";
const inputsDescription = "List inputs in transactions";
const outputsDescription = "List Output utxos in the transaction";
const mintDescription = "Minting Scripts and value in the transaction";
const metadataDescription = "Transaction metadata";
const collateralsDescription =
  "[optional] List of collaterals in the transaction (It is automatically selected if missing)";
const validityStartDescription =
  "[Integer: PosixTimestamp seconds] (convinence field for usage instead of validityStartSlot) Transaction validFrom";
const validityStartSlotDescription =
  "[Integer: Slot Number] Transaction validFrom";
const validityEndDescription =
  "[Integer : PosixTimestamp seconds] (convinence field for usage instead of validityEndSlot) Transaction validUntil";
const validityEndSlotDescription =
  "[Integer : Slot Numbers] Transaction validUntil";
const feeDescription =
  "[Integer : Lovelace] Fee is calculated automatically, but setting this will set transaction fee explicitly.";
const changeAddressDescription =
  "Default change address. If it's missing, it's selected from one of the selection address. Setting addChange in any one output will disable this option";
