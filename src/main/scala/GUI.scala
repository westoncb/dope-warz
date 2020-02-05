import java.awt.{Color, Dimension, Font, Graphics, Graphics2D, Rectangle}

import javax.swing.{JFrame, JOptionPane, JPanel, JSplitPane, WindowConstants}
import java.awt.event.KeyEvent
import java.awt.event.KeyListener

case class GUI(game: Game, width: Int, height: Int) {
    var cursorIndex = 0

    def show: Unit = {

        val frame = new JFrame()
        frame.setSize(width, height)
        frame.setLocationRelativeTo(null)
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

        val panel = new JPanel() {
            override def paintComponent(g: Graphics): Unit = {
                g.setColor(Color.BLACK)
                g.fillRect(0, 0, width, height)
                g.setColor(Color.GREEN)

                drawGame(g, game.state, 15, 15, width - 30, height - 30)
            }
        }

        panel.setFocusable(true)
        panel.requestFocusInWindow

        panel.addKeyListener(new KeyListener() {
            override def keyTyped(e: KeyEvent): Unit = {}
            override def keyReleased(e: KeyEvent): Unit = {}

            override def keyPressed(e: KeyEvent): Unit = {
                if (e.getKeyCode == KeyEvent.VK_UP)
                    cursorIndex = Math.max(cursorIndex - 1, 0)
                else if (e.getKeyCode == KeyEvent.VK_DOWN)
                    cursorIndex = Math.min(cursorIndex + 1, game.state.activeContext.actions.length - 1)
                else if (e.getKeyCode == KeyEvent.VK_ENTER) {

                    game.step(game.state.activeContext.actions(cursorIndex))
                    cursorIndex = 0
                } else if (e.getKeyCode == KeyEvent.VK_ESCAPE)
                    System.exit(0)
            }
        })

        panel.setPreferredSize(new Dimension(width, height))

        frame.setContentPane(panel)
        frame.pack()

        frame.setVisible(true)

        new Thread() {
            override def run(): Unit = {
                while (true) {
                    panel.repaint(new Rectangle(0, 0, width, height))
                    try Thread.sleep(20)
                    catch {
                        case ie: InterruptedException => println(ie)
                    }
                }
            }
        }.start()
    }

    case class Table(width: Int, height: Int, rows: Int, cols: Int, x: Int, y: Int) {
        def colWidth = width / cols
        def rowHeight = height / rows
    }

    def drawGame(g: Graphics, state: GameState, startX: Int, startY: Int, width: Int, height: Int): Unit = {

        g.setFont(new Font("Helvetica", Font.PLAIN, 18))
        g.drawRect(startX, startX, width, height)

        val tableParams = Table(width / 2, height / 2, 5, 2, startX, startY)
        drawTable(g, tableParams)

        var index = 0
        for ((k,v) <- state.player.drugs) {
            drawStringInCell(g, ""+k, tableParams, index, 0)
            drawStringInCell(g, ""+v, tableParams, index, 1)

            index += 1
        }

        drawStringInRect(g, game.state.activeContext.message, new Rectangle(startX + width/2, startY, width/2, height/2))

        val promptString = game.state.activeContext.actionPrompt
        val promptBounds = g.getFontMetrics.getStringBounds(promptString, g)

        g.drawString(promptString, startX + 15, startY + height/2 + promptBounds.getHeight.toInt + 15)

        val actionsTable = Table(width/4, height/4, game.state.activeContext.actions.length, 2, startX, startY + height/2 + (height/4 - height/8))
        for ((action, i) <- game.state.activeContext.actions.zipWithIndex) {
            drawStringInCell(g, action.name, actionsTable, i, 1)
        }

        g.setFont(new Font("Helvetica", Font.PLAIN, 30))
        drawStringInCell(g, ">", actionsTable, cursorIndex, 0)
        g.setFont(new Font("Helvetica", Font.PLAIN, 18))
    }

    def drawTable(g: Graphics, table: Table): Unit = {


        for (i <- 1 to table.cols) {
            g.drawLine(table.x + i * table.colWidth, table.y, table.x + i * table.colWidth, table.y + table.height)
        }
        for (i <- 1 to table.rows) {
            g.drawLine(table.x, table.y + i * table.rowHeight, table.x + table.width, table.y + i * table.rowHeight)
        }
    }

    def drawStringInCell(g: Graphics, string: String, table: Table, row: Int, col: Int): Unit = {
        drawStringInRect(g, string, new Rectangle(table.x + col * table.colWidth, table.y + row * table.rowHeight, table.colWidth, table.rowHeight))
    }

    def drawStringInRect(g: Graphics, string: String, rect: Rectangle): Unit = {
        val parts = string.split("\n")

        val lineBounds = parts.map(str => g.getFontMetrics.getStringBounds(str, g))
        val widths = lineBounds.map(bounds => bounds.getWidth)
        val lineHeight = lineBounds(0).getHeight.toInt
        val widestLine = widths.max.toInt
        val fullBounds = new Rectangle(0, 0, widestLine, (lineHeight + 5) * lineBounds.length)

        val stringBoxX: Int = (rect.x + rect.width/2 - fullBounds.getWidth/2).toInt
        val stringBoxY: Int = (rect.y + rect.height/2 + fullBounds.getHeight/2).toInt

        for ((str, i) <- parts.zipWithIndex) {

            val xPos: Int = stringBoxX
            val yPos: Int = stringBoxY + i * (lineHeight+5)

            g.drawString(str, xPos, yPos)
        }
    }
}
