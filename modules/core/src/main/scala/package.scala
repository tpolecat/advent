import matryoshka.data.Nu
import matryoshka.patterns.EnvT

package object muck {

  // A location is a cursor in our map, annotated by id
  type Location = Nu[LocationF]

}
