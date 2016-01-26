
import scala.math.Ordered._

/**Luokka, joka mallintaa pelikorttia
 * @param m maa
 * @param a arvo
 * AE: (m > 0 && m <= 4) && (a > 0 && a <= 13)*/
class Pelikortti(m: Int, a: Int) extends Ordered[Pelikortti]{
  //metodeja ilman argumentteja (Scala mahdollistaa, että ei tarvita tyhjiä sulkuja kutsussa, kun halutaan pääsy näihin arvoihin)
  var maa = m
  var arvo = a
  
  def compare(that: Pelikortti): Int ={
    this.arvo - that.arvo
  }

  
  override def toString(): String = {
    if (maa == 1){
      "pata" + " " + arvo
    }
    else if (maa == 2){ 
      "hertta" + " " + arvo
    }
    else if (maa == 3){ 
      "risti" + " " + arvo
    }
    else "ruutu" + " " + arvo
  }
}