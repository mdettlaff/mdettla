/*****************************************************************
JADE - Java Agent DEvelopment Framework is a framework to develop 
multi-agent systems in compliance with the FIPA specifications.
Copyright (C) 2002 TILAB

GNU Lesser General Public License

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation, 
version 2.1 of the License. 

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA.
*****************************************************************/

package jade.misc;

import jade.core.Agent;
import jade.core.AID;
import jade.util.Logger;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import java.util.*;

/**
   @author GiovanniCaire - TILAB
 */
class FederationGraphPanel extends JPanel {
	private DFFederatorAgentGUI gui;
	
	private int panelW;
	private int panelH;
	private int r, cx, cy;
	
	private LinkedList dfs = new LinkedList();
	private LinkedList federations = new LinkedList();
	
	private static Logger logger = Logger.getMyLogger(FederationGraphPanel.class.getName());
	
	/** 
	   Constructor 
	 */
	public FederationGraphPanel(final DFFederatorAgentGUI gui) {
		super();
		this.gui = gui;
		panelW = 700;
		panelH = 600;
		r = 260;
		cx = panelW/2;
		cy = panelH/2;
		setPreferredSize(new Dimension(panelW, panelH));
		setBackground(Color.white);
		
		addMouseListener(new MouseAdapter() {
			public void mousePressed(MouseEvent me) {
				if (SwingUtilities.isRightMouseButton(me)) {
					JPopupMenu popup = null;
					AID df = getDFId(me.getPoint());
					if (df != null) {
						// The user clicked on a DF
						popup = new DFMenu(df, gui);
					}
					else {
						Federation f = getFederation(me.getPoint());
						if (f != null) {
							// The user clicked on a Federation arrow
							popup = new FederationMenu(f, gui);
						}
					}
					
					if (popup != null) {
						popup.show(me.getComponent(), me.getX(), me.getY());
					}
				}
			}
		});
	}
	
	/**
	   This is called by the AWT framework to paint the component
	 */
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		
		// Draw all federations
		synchronized (federations) {
			Iterator it = federations.iterator();
			while (it.hasNext()) {
				Federation f = (Federation) it.next();
				f.draw(g);
			}
		}
		
		// Draw all known DFs
		synchronized (dfs) {
			Iterator it = dfs.iterator();
			while (it.hasNext()) {
				DF df = (DF) it.next();
				df.draw(g);
			}
		}					
	}
	
	/**
	   Get the DF AID (if any) whose image includes the indicated 
	   point
	 */
	private AID getDFId(Point p) {
		AID selectedDF = null;
		Iterator it = dfs.iterator();
		while (it.hasNext()) {
			DF df = (DF) it.next();
			if (df.contains(p)) {
				selectedDF = df.getID();
				break;
			}
		}
		return selectedDF;
	}
		
	/**
	   Get the DF object (if any) corresponding to the specified AID
	 */
	private DF getDF(AID id) {
		Iterator it = dfs.iterator();
		while (it.hasNext()) {
			DF df = (DF) it.next();
			if (df.compareID(id)) {
				return df;
			}
		}
		return null;
	}
		
	/**
	   Add a new DF object to the list of known dfs 
	 */
	void addDF(AID id) {
		// Mutual exclusion with paintComponent()
		synchronized (dfs) {
			DF newDf = new DF(id);
			if (!dfs.contains(newDf)) {
				dfs.add(newDf);
				updateDFPositions();
			}
		}
		
		// Let AWT repaint the panel
		repaint();
	}
	
	/**
	   Remove a DF object from the list of known dfs 
	 */
	void removeDF(AID id) {
		if (!isFederated(id)) {
			// Mutual exclusion with paintComponent()
			synchronized (dfs) {
				dfs.remove(new DF(id));
				updateDFPositions();
			}
			
			// Let AWT repaint the panel
			repaint();
		}
		else {
			logger.log(Logger.INFO,"Remove the federations this DF is involved in first.");
			// FIXME: Show a proper dialog
		}			
	}

	/**
	   Recalculate the positions of DF objects so that they are 
	   uniformly distributed within the federation graph panel 
	 */
	private void updateDFPositions() {
		int size = dfs.size();
		if (size > 0) {
			double teta = 2*Math.PI/size;
			for (int i = 0; i < size; ++i) {
				DF df = (DF) dfs.get(i);
				df.setX((int) (cx + r * Math.cos(i*teta)));
				df.setY((int) (cy - r * Math.sin(i*teta)));
			}
		}
	}
	
	private boolean isFederated(AID id) {
		Iterator it = federations.iterator();
		while (it.hasNext()) {
			Federation f = (Federation) it.next();
			if (f.getChild().equals(id) || f.getParent().equals(id)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	   Get the Federation (if any) whose arrow includes the indicated 
	   point
	 */
	private Federation getFederation(Point p) {
		Federation selectedF = null;
		Iterator it = federations.iterator();
		while (it.hasNext()) {
			Federation f = (Federation) it.next();
			if (f.contains(p)) {
				selectedF = f;
				break;
			}
		}
		return selectedF;
	}
		
	/**
	   Add a new Federation to the list of known federations 
	 */
	void addFederation(AID childDF, AID parentDF) {
		synchronized (federations) {
			Federation f = new Federation(childDF, parentDF);
			if (!federations.contains(f)) {
				federations.add(f);
			}
		}
		
		// Let AWT repaint the panel
		repaint();
	}
	
	/**
	   Remove a Federation from the list of known federations 
	 */
	void removeFederation(AID child, AID parent) {
		// Mutual exclusion with paintComponent()
		synchronized (federations) {
			federations.remove(new Federation(child, parent));
		}
		
		// Let AWT repaint the panel
		repaint();
	}

	/**
	   Inner class DF.
	   Represents the association (DF-id, Position)
	 */
	private static class DF {
		private static final int R = 30;
		private AID myID;
		private int x, y;
		
		DF(AID id) {
			myID = id;
		}
		
		AID getID() {
			return myID;
		}
		
		void setX(int x) {
			this.x = x;
		}
		
		void setY(int y) {
			this.y = y;
		}
		
		boolean contains(Point p) {
			if (p.x >= x-R && p.x <= x+R &&
				  p.y >= y-R && p.y <= y+R) {
				return true;
			}
			else {
				return false;
			}				  
		}
		
		boolean compareID(AID id) {
			return (myID.equals(id));
		}
		
		void draw(Graphics g) {
			g.setColor(Color.red);
			g.fillOval(x-R, y-R, 2*R, 2*R);
			g.setColor(Color.black);
			g.drawOval(x-R, y-R, 2*R, 2*R);
			
			byte[] label = myID.getName().getBytes();
			FontMetrics fm = g.getFontMetrics();
			int h = fm.getAscent();
			int w = fm.bytesWidth(label, 0, label.length);
			g.drawBytes(label, 0, label.length, x-w/2, y+R+h); 
		}
		
		public boolean equals(Object obj) {
			if (obj instanceof DF) {
				return myID.equals(((DF) obj).getID());
			}
			else {
				return false;
			}
		}
		
		public int hashCode() {
			return myID.hashCode();
		}
	}
	
	
	/** 
	   Inner class Federation
	 */
	private class Federation {
		private AID parent, child;
		
		Federation(AID child, AID parent) {
			this.child = child;
			this.parent = parent;
		}
		
		AID getParent() {
			return parent;
		}
		
		AID getChild() {
			return child;
		}
		
		void draw(Graphics g) {
			DF ch = getDF(child);
			DF pt = getDF(parent);
			g.setColor(Color.blue);
			g.drawLine(ch.x, ch.y, pt.x, pt.y);
			
			Polygon pl = getArrow(ch.x, ch.y, pt.x, pt.y);			
			g.drawPolygon(pl);
			g.fillPolygon(pl);
		}
		
		private Polygon getArrow(int childx, int childy, int parentx, int parenty) {
			int distX = childx - parentx;
			int distY = childy - parenty;
			int signX = distX / Math.abs(distX);
			int signY = distY / Math.abs(distY);
			double dist = Math.sqrt(distX*distX +distY*distY);
			double ratioX = Math.abs(distX) / dist;
			double ratioY = Math.abs(distY) / dist;
			int H = 16;
			int B = 8;
			
			double vX = ((childx + 3*parentx) / 4);
			double vY = ((childy + 3*parenty) / 4);
			double bX = vX + signX*H*ratioX;
			double bY = vY + signY*H*ratioY;
			
			int b1X = (int) (bX + signY*B*ratioY);
			int b1Y = (int) (bY - signX*B*ratioX);
			int b2X = (int) (bX - signY*B*ratioY);
			int b2Y = (int) (bY + signX*B*ratioX);
			return new Polygon(new int[]{(int) vX, b1X, b2X}, new int[]{(int) vY, b1Y, b2Y}, 3);
		}
		
		boolean contains(Point p) {
			DF ch = getDF(child);
			DF pt = getDF(parent);
			Polygon pl = getArrow(ch.x, ch.y, pt.x, pt.y);		
			return pl.contains(p);
		}
		
		public boolean equals(Object obj) {
			if (obj instanceof Federation) {
				Federation f = (Federation) obj;
				return (child.equals(f.child) && parent.equals(f.parent));
			}
			else {
				return false;
			}
		}		
	}

	/** 
	   Inner class DFMenu
	 */
	private class DFMenu extends JPopupMenu {
		
		DFMenu(AID selectedDF, DFFederatorAgentGUI g) {
			super();
			
			add(new SetAsChildDFAction(selectedDF, g));
			add(new SetAsParentDFAction(selectedDF, g));
			add(new RemoveDFAction(selectedDF, g));
			add(new ShowGUIAction(selectedDF, g));
		}
	}	
	
	/** 
	   Inner class FederationMenu
	 */
	private class FederationMenu extends JPopupMenu {
		
		FederationMenu(Federation f, DFFederatorAgentGUI g) {
			super();
			
			add(new RemoveFederationAction(f.getChild(), f.getParent(), g));
		}
	}	
}
