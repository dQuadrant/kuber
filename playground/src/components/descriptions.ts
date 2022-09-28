import { KuberEnums } from "@/models/enums/DescriptionEnum";

export default class Description {
  static kuberDescription(word: String): String {
    switch (word) {
      case KuberEnums.Selections:
        return "Wallet Selections";
      default:
        return "No descriptions found";
    }
  }
}
