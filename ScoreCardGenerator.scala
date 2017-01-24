import scala.collection.immutable.Iterable

/*Now, I require a case class named ScoreCard having fields (studentId: Long, marks: Map[Long, Float], percentage: Float).
  Write a method which takes no parameter and generates a Map with key student name and value as ScoreCard. As there can be more than one student with same name, the logic we have to follow is that, if two or more student has same name the key shold be the name of the student and the values (ScoreCard s) should be in a List, otherwise the key should be the student name and value should be the case class ScoreCard. e.g. Map should be Map[String, AnyRef].*/
/*Write a method which takes input as student name and print the score cards. If it finds one or more than one score card  print all of them other wise print "No data found". The print should be in increasing order of the student id.*/
case class Student(id: Int, name: String, gender: Gender.Value)

case class Marks(subjectId: Int, studentId: Int, marksObtained: Float) {
  def isPass(percentage: Int): Boolean = {
    if (marksObtained >= percentage) true else false
  }
}

case class ScoreCard(studentId: Int, marks: Map[Int, Float], percentage: Float)

object ScoreCardGenerator extends App {
  val marksList = List(Marks(1, 1, 78), Marks(1, 2, 66), Marks(1, 3, 89), Marks(1, 4, 78), Marks(1, 5, 90), Marks(1, 6, 54), Marks(1, 7, 40), Marks(1, 8, 70), Marks(1, 9, 35), Marks(1, 10, 60), Marks(5, 1, 78), Marks(5, 2, 67), Marks(5, 3, 89), Marks(5, 4, 78), Marks(5, 5, 90), Marks(5, 6, 54), Marks(5, 7, 40), Marks(5, 8, 70), Marks(5, 9, 35), Marks(5, 10, 60), Marks(2, 1, 78), Marks(2, 2, 67), Marks(2, 3, 89), Marks(2, 4, 78), Marks(2, 5, 90), Marks(2, 6, 54), Marks(2, 7, 40), Marks(2, 8, 70), Marks(2, 9, 35), Marks(2, 10, 60), Marks(3, 1, 78), Marks(3, 2, 67), Marks(3, 3, 89), Marks(3, 4, 78), Marks(3, 5, 90), Marks(3, 6, 54), Marks(3, 7, 40), Marks(3, 8, 70), Marks(3, 9, 35), Marks(3, 10, 60), Marks(4, 1, 78), Marks(4, 2, 67), Marks(4, 3, 89), Marks(4, 4, 78), Marks(4, 5, 90), Marks(4, 6, 54), Marks(4, 7, 40), Marks(4, 8, 70), Marks(4, 9, 35), Marks(4, 10, 60))

  val studentList = List(Student(1, "Kunal", Gender.Male), Student(2, "Kunal", Gender.Male), Student(3, "Anmol Mehta", Gender.Male), Student(4, "Geetika", Gender.Female), Student(5, "Mahesh", Gender.Male), Student(6, "Simarpreet", Gender.Female), Student(7, "Ramandeep", Gender.Female), Student(8, "Anuj", Gender.Male), Student(9, "Jatin", Gender.Male), Student(10, "Pankhuri", Gender.Female))
  val marksMapList = fillMarksMap(marksList)

  //Generating the Marks Map[Int,Float], mapping subject id with marks of student
  def fillMarksMap(marksList: List[Marks]):List[(Int,Map[Int,Float],Float)] = {
    val temp = marksList.groupBy(x => x.studentId)
    val marksMapList = temp.map(x => (x._1, x._2.map(y => (y.subjectId, y.marksObtained)).toMap, x._2.map(y => y.marksObtained).sum / 5))
    marksMapList.toList
  }

  //Getting the scorecard for the particular student
  def getStudentScoreCard(studentName: String) = {
    val finalMap = createStudentScoreCardMap()
    if (finalMap.contains(studentName)) {
      val res = finalMap.get(studentName)
      res match {
        case Some(x) => println(x)
        case None => println("No value found")
      }
    }
    else
      println("No data found")
  }

  //Generating the map in the required format handling the duplicate keys
  def createStudentScoreCardMap() = {
    val resultList = displayScoreCard()

    val resultMap = resultList.groupBy(x => x._1)
    resultMap
  }

  def displayScoreCard():List[(String, ScoreCard)] = {
    val scoreCardList = generateScoreCards()
    val resMap = marksMapList.map(x => ((for (student <- studentList if (x._1 == student.id)) yield student.name).zip(for (scoreCard <- scoreCardList if (x._1 == scoreCard.studentId)) yield scoreCard))).flatten
    resMap
  }

  //Generating scorecards
  def generateScoreCards():List[ScoreCard] = {
    val scoreCardList = marksMapList.map(x => (ScoreCard(x._1, x._2, x._3)))
    scoreCardList
  }



  /* Write a method getScoreCardByGender to return a tuple of ScoreCards (e.g. (List[ScoreCard], List[ScoreCard])),
  where first field in the tuple has male student's score card and the second field has female student's score cards.
   */

  def getMoreThan50Result():(List[ScoreCard],List[ScoreCard])= {
    val tuple = fillMarksMapByGender()
    val res1 = tuple._1.filter(x => x.percentage > 50.0)
    val res2 = tuple._2.filter(x=>x.percentage > 50.0)
    (res1,res2)
  }

  def fillMarksMapByGender():(List[ScoreCard],List[ScoreCard]) = {
    val scoreCardList = generateScoreCards()
    val scoreCardMaleList = for (scoreCard <- scoreCardList; student <- studentList if (scoreCard.studentId == student.id && student.gender == Gender.Male)) yield scoreCard
    val scoreCardFemaleList = for (scoreCard <- scoreCardList; student <- studentList if (scoreCard.studentId == student.id && student.gender == Gender.Female)) yield scoreCard
    (scoreCardMaleList, scoreCardFemaleList)
  }

  def findSimilarPercentage(list1: List[ScoreCard], list2: List[ScoreCard]): List[((String, Float),(String, Float))] = {

    val res = for (el <- list1; el2 <- list2; maleStudent <- studentList; femaleStudent<-studentList; if ((el.percentage == el2.percentage)&& ((maleStudent.id == el.studentId)&&(femaleStudent.id == el2.studentId)))) yield {
      ((maleStudent.name, el.percentage),(femaleStudent.name, el2.percentage))
    }
    res
  }

  def getDifferenceMarks() = {
    val resList = fillMarksMapByGender()
    val similarPercentList = findSimilarPercentage(resList._1, resList._2)
    val difference =  resList._2.diff(similarPercentList.map(x=> x._2))
    for(res<-difference; student<-studentList if(res.studentId==student.id)) yield (student.name, res.percentage)
  }

  println("MarksMapList : " + marksMapList+"\n")
  println("ScoreCardList : " + generateScoreCards() +"\n")

  getStudentScoreCard("Kunal")
  getStudentScoreCard("Simar")

  val genderList = fillMarksMapByGender()
  println("Male gender List \n" + genderList._1+"\n")
  println("Female gender List \n"+ genderList._2+"\n")

  val moreThan50Percent = getMoreThan50Result()
  println("Male students with ScoreCard more than 50% result \n" + moreThan50Percent._1+"\n")
  println("Female students with ScoreCard more than 50% result \n" + moreThan50Percent._2+"\n")

  println("Similar Marks Students : "+ findSimilarPercentage(genderList._1, genderList._2)+"\n")
  println("Percentage in girls list but not in boys list : " + getDifferenceMarks())
}


