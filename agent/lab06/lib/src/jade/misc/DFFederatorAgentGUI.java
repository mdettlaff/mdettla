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
import jade.gui.AIDGui;
import jade.util.Logger;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
   @author GiovanniCaire - TILAB
 */
class DFFederatorAgentGUI extends JFrame {
	private DFFederatorAgent myAgent;
	private FederationGraphPanel graphPanel;
	private JTextField childDFTxt, parentDFTxt;
	private AID childDF, parentDF;
	
	private static Logger logger = Logger.getMyLogger(DFFederatorAgentGUI.class.getName());
	/** 
	   Constructor 
	 */
	public DFFederatorAgentGUI(DFFederatorAgent a) {
		super();
		setTitle("DF Federator");
		myAgent = a;
		
		//////////////////////////////////////////////
		// Add Toolbar to the NORTH part of the GUI  
		//////////////////////////////////////////////
		JToolBar bar = new JToolBar();

		// EXIT 
		JButton exitB = bar.add(new ExitAction(this));
		exitB.setText("Exit");
		exitB.setToolTipText("Exit");
		bar.add(exitB);
		
		// ADD DF
		JButton addDFB = bar.add(new AddDFAction(this));
		addDFB.setText("Add DF");
		addDFB.setToolTipText("Add a DF agent to the list of known DFs");
		bar.add(addDFB);

		// FEDERATE
		JButton federateB = bar.add(new FederateAction(this));
		federateB.setText("Federate");
		federateB.setToolTipText("Federate the selected child DF with the selected parent DF");
		bar.add(federateB);

		addDFB.setPreferredSize(federateB.getPreferredSize());
		exitB.setPreferredSize(federateB.getPreferredSize());
		
		// Child DF 
		bar.addSeparator();
		bar.add(new JLabel("Child-DF: "));
		childDFTxt = new JTextField();
		childDFTxt.setEditable(false);
		bar.add(childDFTxt);

		// Parent DF 
		bar.addSeparator();
		bar.add(new JLabel("Parent-DF: "));
		parentDFTxt = new JTextField();
		parentDFTxt.setEditable(false);
		bar.add(parentDFTxt);

		getContentPane().add(bar, BorderLayout.NORTH);
		
		
		///////////////////////////////////////////////////////////////
		// Add the Federation Graph Panel to the CENTER part of the GUI
		///////////////////////////////////////////////////////////////
		graphPanel = new FederationGraphPanel(this);
		JScrollPane sp = new JScrollPane(graphPanel);
		getContentPane().add(sp, BorderLayout.CENTER);
		
		
		////////////////////////////////////////////////////////////////
		// Execute the EXIT action when the user attempts to close 
		// the GUI using the button on the upper right corner	
		////////////////////////////////////////////////////////////////
		addWindowListener(new	WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				ExitAction ac = new ExitAction(DFFederatorAgentGUI.this);
				ac.actionPerformed(new ActionEvent((Object) this, 0, "Exit"));
			}
		} );
	}
	

	////////////////////////////////////////////
	// User action methods
	////////////////////////////////////////////
	/**
	   This is called by the ExitAction when it is executed
	 */
	void exit() {
		myAgent.doDelete();
	}
	
	/**
	   This is called by the AddDFAction when it is executed
	 */
	void addDF() {
		// Show the AID dialog to get the DF AID
		AIDGui dlg = new AIDGui(this);
		AID df = dlg.ShowAIDGui(null, true, true);
		
		graphPanel.addDF(df);
	}
	
	/**
	   This is called by the FederateAction when it is executed
	 */
	void federate() {
		if (childDF != null && parentDF != null) {
			myAgent.requestFederation(childDF, parentDF);
		}
	}
	
	/**
	   This is called by the RemoveFederationAction when it is executed
	 */
	void removeFederation(AID child, AID parent) {
		myAgent.requestFederationRemoval(child, parent);
	}
	
	/**
	   This is called by the RemoveDFAction when it is executed
	 */
	void removeDF(AID df) {
		if (df.equals(childDF)) {
			childDF = null;
			childDFTxt.setText(null);
		}
		if (df.equals(parentDF)) {
			parentDF = null;
			parentDFTxt.setText(null);
		}
		
		graphPanel.removeDF(df);
	}
	
	/**
	   This is called by the SetAsChildDFAction when it is executed
	 */
	void setChildDF(AID df) {
		childDF = df;
		childDFTxt.setText(df.getName());
	}
	
	/**
	   This is called by the SetAsParentDFAction when it is executed
	 */
	void setParentDF(AID df) {
		parentDF = df;
		parentDFTxt.setText(df.getName());
	}	
	
	/**
	   This is called by the ShowGUIAction when it is executed
	 */
	void showGUI(AID df) {
		myAgent.requestShowGui(df);
	}	
	
	////////////////////////////////////////////
	// Methods called by the DF Federator Agent
	////////////////////////////////////////////
	void addDF(AID id) {
		graphPanel.addDF(id);
	}
	
	void notifyFederationOK(AID childDF, AID parentDF) {
		logger.log(Logger.INFO,"Federation between "+childDF.getName()+" and "+parentDF.getName()+" OK");
		graphPanel.addFederation(childDF, parentDF);
	}
	
	void notifyFederationRemoved(AID childDF, AID parentDF) {
		logger.log(Logger.INFO,"Federation between "+childDF.getName()+" and "+parentDF.getName()+" removed");
		graphPanel.removeFederation(childDF, parentDF);
	}
	
	void notifyFailure(String msg) {
		logger.log(Logger.WARNING,msg);
		// FIXME: Show an error dialog
	}
	
	
	/**
	   Show the DFFederatorAgentGUI properly
	 */
	void showCorrect() {
		pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		int centerX = (int)screenSize.getWidth() / 2;
		int centerY = (int)screenSize.getHeight() / 2;
		setLocation(centerX - getWidth() / 2, centerY - getHeight() / 2);
		setVisible(true);
	}
	
}
