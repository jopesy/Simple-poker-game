import scala.util.Sorting
import scala.math.Ordering._
import scala.math.Ordered._
import scala.collection.mutable.ArrayBuffer

/**Luokka, joka mallintaa viiden kortin pokerikättä*/
class Pokerikasi extends Ordered[Pokerikasi]{
  
  var pokerikasi = new ArrayBuffer[Pelikortti]()
  var kategoria: Int = 0
  var merkitsevaArvo: Int = 0
  var merkitsevaArvo2: Int = 0
  
  //Käsien kategoriat:
  final val HAI: Int = 0
  final val PARI: Int = 1
  final val KAKSI_PARIA: Int = 2
  final val KOLMOSET: Int = 3
  final val SUORA: Int = 4
  final val VARI: Int = 5
  final val TAYSKASI: Int = 6
  final val NELOSET: Int = 7
  final val VARISUORA: Int = 8

  def annaKortti(indeksi: Int): Pelikortti ={
    return pokerikasi(indeksi)
  }
  def lisaaKateen(kortti: Pelikortti) ={
    pokerikasi += kortti
  }
  def poistaKadesta(indeksi: Int) = {
    pokerikasi.remove(indeksi)
  }
  /**Järjestää käden kortit suuruusjärjestykseen*/
  def jarjestaKasi() ={
    pokerikasi = pokerikasi.sortBy(_.arvo)
  }
  
  /**Palauttaa arvon 0-8 käden kategorian mukaan*/
  def annaKategoria(): Int = {
    var arvo = HAI // var, koska sitä voi muuttaa
    if(yksiPari() == true){
      arvo = PARI
    }
    if(kaksiParia() == true){
      arvo = KAKSI_PARIA
    }
    if(kolmoset() == true){
      arvo = KOLMOSET
    }
    if(suora() == true){
      arvo = SUORA
    }
    if(vari() == true){
      arvo = VARI
    }
    if(tayskasi() == true){
      arvo = TAYSKASI
    }
    if(neloset() == true){
      arvo = NELOSET
    }
    if(varisuora() == true){
      arvo = VARISUORA
    }
    return arvo
    
  }
  
  //Yksi Pari
  def yksiPari(): Boolean ={
    for (i <- 2 to 14){
      var samoja: Int = 0
      for (j <- 0 to 4){
        if (i == pokerikasi(j).arvo){
          samoja = samoja +1 //Jos jotain arvoa (2-14) löytyy kädestä, muuttujaa samoja kasvatetaan yhdellä
        }
      }
      if (samoja == 2){ //Jos jotain arvoa (2-14) löytyy kädestä tasan 2 kpl, palauttaa true
          this.merkitsevaArvo = i //Ensisijainen merkitsevä arvo on parin muodostavien korttien arvo
          this.merkitsevaArvo2 = i-2 //Toissijainen merkitsevä arvo on suurimman käden ulkopuolisen kortin arvo
          return true
      }
    }
    return false
  }
  
  //Kaksi Paria
  def kaksiParia(): Boolean ={
    var pareja: Int = 0;
    for(i <- 2 to 14){
      var samoja: Int = 0;
      for(j <- 0 to 4){
        if(i == pokerikasi(j).arvo){
          samoja = samoja +1;
        }
      }
      if(samoja == 2){
        pareja = pareja +1;
      }
    }
    if(pareja == 2){
      return true;
    }
    return false;
  }
  
  //Kolmoset
  def kolmoset(): Boolean ={
    for (i <- 2 to 14){
      var samoja: Int = 0
      for (j <- 0 to 4){
        if (i == pokerikasi(j).arvo){
          samoja = samoja +1 //Jos jotain arvoa (2-14) löytyy kädestä, muuttujaa samoja kasvatetaan yhdellä
        }
      }
      if (samoja == 3){ //Jos jotain arvoa (2-14) löytyy kädestä 3 kpl, palauttaa true
        return true
      }
    }
    return false; 
  }
  
  //Suora (toimii, sillä käden kortit on järjestetty suuruusjärjestykseen)
  def suora(): Boolean ={
    for (i <- 0 to 3){
      if(pokerikasi(i+1).arvo != pokerikasi(i).arvo+1){ //Jos arvoltaan suurempi kortti ei ole yhtä suurempi kuin edellinen, palauttaa false
        return false
      }
    }
    if(pokerikasi(4).arvo == pokerikasi(3).arvo+1){
      return true
    }
    return false
  }
  
  //Väri
  def vari(): Boolean ={
    for(i <- 1 to 4){ //i = joku maa
      var samoja: Int = 0
      for(j <- 0 to 4){
        if(i == pokerikasi(j).maa){
          samoja = samoja +1
        }
      }
      if(samoja == 5){
        return true;
      }
    }
    return false;
  }
  
  //Täyskäsi
  def tayskasi(): Boolean ={
    if(yksiPari() == true && kolmoset() == true){ //Mikäli kädestä löytyy sekä kolmoset että yksi pari
      return true
    }
    return false
  }
  
  //Neloset
  def neloset(): Boolean ={
    for (i <- 2 to 14){
      var samoja: Int = 0
      for (j <- 0 to 4){
        if (i == pokerikasi(j).arvo){
          samoja = samoja +1 //Jos jotain arvoa (2-14) löytyy kädestä, muuttujaa samoja kasvatetaan yhdellä
        }
      }
      if (samoja == 4){ //Jos jotain arvoa (2-14) löytyy kädestä 4 kpl, palauttaa true
        return true
      }
    }
    return false; 
  }
  
  //Värisuora
  def varisuora(): Boolean ={
    if(suora() == true && vari() == true){ //Mikäli kädestä löytyy sekä suora että väri
      return true
    }
    return false
  }
  
  //LISÄÄ KORTTIEN NUMEROARVOJEN MERKITYS!!
  def compare(that: Pokerikasi): Int ={
    if(this.annaKategoria() == that.annaKategoria()){ //Jos saman kategorian kädet
      if(this.merkitsevaArvo > that.merkitsevaArvo){
        return 1
      }
      else if(this.merkitsevaArvo < that.merkitsevaArvo){
        return -1
      }
      else{
        if(this.merkitsevaArvo2 > that.merkitsevaArvo2){
          return 1
        }
        else if(this.merkitsevaArvo < that.merkitsevaArvo){
          return -1
        }
        else return 0
      }
    }
    else {
      this.annaKategoria() compare that.annaKategoria()
    }
  }
  
  override def toString(): String = {
    pokerikasi(0).toString() +", "+ pokerikasi(1).toString() +", "+ pokerikasi(2).toString() +", "+
    pokerikasi(3).toString() +" ja "+ pokerikasi(4).toString()
  }
}