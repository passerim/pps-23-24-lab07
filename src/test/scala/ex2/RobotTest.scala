package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotTest extends AnyFlatSpec with Matchers:
  "A SimpleRobot" should "turn correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)
    // Action
    robot.turn(Direction.East)
    robot.direction should be(Direction.East)
    // Action
    robot.turn(Direction.South)
    robot.direction should be(Direction.South)
    // Action
    robot.turn(Direction.West)
    robot.direction should be(Direction.West)
    // Action
    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)
    // Action
    robot.act()
    robot.position should be((0, 1))
    // Action
    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))
    // Action
    robot.turn(Direction.South)
    robot.act()
    robot.position should be((1, 0))
    // Action
    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))

  "A DumbRobot" should "never turn" in {
    val robot = DumbRobot(SimpleRobot((0, 0), Direction.North))
    robot.turn(Direction.East)
    robot.direction should be(Direction.North)
  }

  "A RobotWithBattery" should "stop acting after battery is discharged" in:
    val batteryCharge = 5
    val robot         = RobotWithBattery(SimpleRobot((0, 0), Direction.North), batteryCharge)
    for _ <- 0 to batteryCharge do
      robot.turn(robot.direction.turnRight)
      robot.act()
    val robotPosition = robot.position
    robot.turn(robot.direction.turnRight)
    robot.act()
    robot.position should be(robotPosition)

  "A RobotCanFail" should "never act when fail probability is 1" in:
    val failProbability = 1.0
    val robot           = RobotCanFail(SimpleRobot((0, 0), Direction.North), failProbability)
    val robotPosition   = robot.position
    robot.act()
    robot.position should be(robotPosition)

  it should "always act when fail probability is 0" in:
    val failProbability = 0.0
    val robot           = RobotCanFail(SimpleRobot((0, 0), Direction.North), failProbability)
    val robotPosition   = robot.position
    robot.act()
    robot.position should not be robotPosition

  "A RobotRepeated" should "repeat an action for the given times" in:
    val repeat      = 5
    val simpleRobot = SimpleRobot((0, 0), Direction.North)
    val robot       = RobotRepeated(SimpleRobot((0, 0), Direction.North), repeat)
    for _ <- 0 to repeat do simpleRobot.act()
    robot.act()
    robot.position should be(simpleRobot.position)
