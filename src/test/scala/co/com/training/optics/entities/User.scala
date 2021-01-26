package co.com.training.optics.entities

case class User(name: String, cart: Cart)
case class Cart(id: String, item: Item, quantity: Int)
case class Item(sku: String, price: Double, leftInStock: Int, discount: Discount)