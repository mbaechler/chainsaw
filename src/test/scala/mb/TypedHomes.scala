package mb

enum TypedHomes:
  case `🏘️`
  case 🏡
  case 🌳
  case 🌴
  case 🌲

given Show[TypedHomes] = {
  case TypedHomes.`🏘️` => "🏘️"
  case TypedHomes.🏡    => "🏡"
  case TypedHomes.🌳    => "🌳"
  case TypedHomes.🌴    => "🌴"
  case TypedHomes.🌲    => "🌲"
}
