import java.util.Scanner

object Pokerisimulaatio{
  def main(args: Array[String]){
    var sc: Scanner = new Scanner(System.in)
    println("Montako pelaajaa? (Enintään 5)")
    var pelaajia = sc.nextLine().toInt
    println("Aloitetaan "+pelaajia+":n pelaajan peli...")
    var peli: Pokeripeli = new Pokeripeli()
    lisaaPelaajat()
    peli.jaaKortit()
    tulostaKadet()
    vaihdaKortteja()
    tulostaKadet()
    sc.close()
    println("Pelin voittaa "+ peli.annaVoittaja() +" kädellä: " + peli.annaVoittaja().annaKasi()) //Lisää voittokäden kategoria tulokseen
    
    def tulostaKadet() ={
      for(i <- 0 to peli.pelaajat.size-1){
        var pelaaja = peli.pelaajat(i)
        println(pelaaja.toString()+":n käsi on: "+ pelaaja.annaKasi())
      }
    }
    
    /**Korttien vaihtaminen*/ //Kortit täytyy poistaa kädestä takaperin, jotteivät indeksit mene sekaisin!!
    def vaihdaKortteja() ={
      for(i <- 0 to peli.pelaajat.size-1){
        println(peli.pelaajat(i).toString()+", haluatko vaihtaa kortteja? (K/E)")
        var vaihto = sc.nextLine()
        if(vaihto=="k"){
          println("Mitkä kortit (1-5) haluat vaihtaa? (esim. 234 vaihtaa kortit 2, 3 ja 4)")
          var vaihdettavat = sc.nextLine()
          if(vaihdettavat contains "5"){
            peli.vaihdaKortti(peli.pelaajat(i), 4)
          }
          if(vaihdettavat contains "4"){
            peli.vaihdaKortti(peli.pelaajat(i), 3)
          }
          if(vaihdettavat contains "3"){
            peli.vaihdaKortti(peli.pelaajat(i), 2)
          }
          if(vaihdettavat contains "2"){
            peli.vaihdaKortti(peli.pelaajat(i), 1)
          }
          if(vaihdettavat contains "1"){
            peli.vaihdaKortti(peli.pelaajat(i), 0)
          }
        }
        else println("ei vaihtoja")
      }
      var pelaajat = peli.pelaajat
      for(i <- 0 to pelaajat.size-1){
        pelaajat(i).annaKasi.jarjestaKasi() //Järjestää pelaajien kädet uudestaan
      }
    }
    
    /**Lisää luotuun peliin käyttäjän syöttämän määrän pelaajia*/
    def lisaaPelaajat() = pelaajia match{
      case 1 => peli.lisaaPelaaja(new Pelaaja("Pelaaja 1"))
      case 2 => peli.lisaaPelaaja(new Pelaaja("Pelaaja 1"))
        peli.lisaaPelaaja(new Pelaaja("Pelaaja 2"))
      case 3 => peli.lisaaPelaaja(new Pelaaja("Pelaaja 1"))
        peli.lisaaPelaaja(new Pelaaja("Pelaaja 2"))
        peli.lisaaPelaaja(new Pelaaja("Pelaaja 3"))
      case 4 => peli.lisaaPelaaja(new Pelaaja("Pelaaja 1"))
        peli.lisaaPelaaja(new Pelaaja("Pelaaja 2"))
        peli.lisaaPelaaja(new Pelaaja("Pelaaja 3"))
        peli.lisaaPelaaja(new Pelaaja("Pelaaja 4"))
      case 5 => peli.lisaaPelaaja(new Pelaaja("Pelaaja 1"))
        peli.lisaaPelaaja(new Pelaaja("Pelaaja 2"))
        peli.lisaaPelaaja(new Pelaaja("Pelaaja 3"))
        peli.lisaaPelaaja(new Pelaaja("Pelaaja 4"))
        peli.lisaaPelaaja(new Pelaaja("Pelaaja 5"))
    }
  }
}