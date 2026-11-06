package arithmetic

enum ColorWithThreeValues:
  case Named(name: String, intensity: Int, priority: Int)
  case Custom(red: Int, green: Int, blue: Int)
