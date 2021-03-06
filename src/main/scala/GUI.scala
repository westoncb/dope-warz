import java.awt.{Color, Dimension, Font, Graphics, Rectangle}

import javax.swing.{JFrame, JPanel, WindowConstants}
import java.awt.event.KeyEvent
import java.awt.event.KeyListener

case class GUI(game: Game, width: Int, height: Int) {
    var cursorIndex = 0

    def show: Unit = {

        val frame = new JFrame("Dope Warz")
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
                    cursorIndex = if (cursorIndex - 1 >= 0) cursorIndex - 1 else game.state.activeContext.actions.length-1
                else if (e.getKeyCode == KeyEvent.VK_DOWN)
                    cursorIndex = (cursorIndex + 1) % game.state.activeContext.actions.length
                else if (e.getKeyCode == KeyEvent.VK_ENTER) {

                    game.step(game.state.activeContext.actions(cursorIndex))

                    if (!game.state.lockCursor)
                        cursorIndex = 0

                } else if (e.getKeyCode == KeyEvent.VK_ESCAPE) {

                    game.state.activeContext.actions.find(action => {
                        action.name.toLowerCase().equals("leave") ||
                            action.name.toLowerCase().equals("go back") ||
                            action.name.toLowerCase().equals("exit")
                    }) match {
                        case Some(action) => game.step(action)
                        case None =>
                    }
                }
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
        def colWidth = Math.ceil(width.toFloat / cols).toInt
        def rowHeight = Math.ceil(height.toFloat / rows).toInt
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

        g.setFont(new Font("Helvetica", Font.PLAIN, 24))
        g.drawString(promptString, startX + 15, startY + height/2 + promptBounds.getHeight.toInt + 15)

        val actionsTable = Table(width/4, height/4, game.state.activeContext.actions.length, 2, startX, startY + height/2 + (height/4 - height/8))
        for ((action, i) <- game.state.activeContext.actions.zipWithIndex) {
            drawStringInCell(g, action.name, actionsTable, i, 1)
        }

        g.setFont(new Font("Helvetica", Font.PLAIN, 30))
        drawStringInCell(g, ">", actionsTable, cursorIndex, 0)
        g.setFont(new Font("Helvetica", Font.PLAIN, 18))

        game.state.activeContext match {
            case MarketContext(gameState) => {
                val tableWidth = width / 4
                val tableHeight = height / 4
                val marketTable = Table(tableWidth, tableHeight, 5, 2, startX + width/2 + width/4 - tableWidth/2, startY + height/2 + height/4 - tableHeight/2)
                drawTable(g, marketTable)

                var index = 0
                for ((k,v) <- gameState.drugPrices) {
                    drawStringInCell(g, ""+k, marketTable, index, 0)
                    drawStringInCell(g, "$"+v, marketTable, index, 1)

                    index += 1
                }
            }
            case _ =>
        }

        var status = s"Money: ${"$" + game.state.player.money}   |   Bank: ${"$" + game.state.player.bankMoney}   |   Debt: ${"$" + game.state.player.debt}   |   Date: ${game.state.date}"

        if (game.state.player.debt > 0) {
            status += s" (${game.state.turnsRemaining} days left)"
        }

        val bounds = g.getFontMetrics.getStringBounds(status, g)
        g.drawString(status, startX + width - bounds.getWidth.toInt - 10, startY + height - bounds.getHeight.toInt + 10)

        g.drawString(game.state.region.name, startX + 10, startY + height - bounds.getHeight.toInt + 10)
    }

    def drawTable(g: Graphics, table: Table): Unit = {


        for (i <- 0 to table.cols) {
            g.drawLine(table.x + i * table.colWidth, table.y, table.x + i * table.colWidth, table.y + table.height)
        }
        for (i <- 0 to table.rows) {
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
