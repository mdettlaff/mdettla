package jade.misc;

import jade.core.Agent;
import jade.core.AID;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


public class FieldViewerAgent extends Agent {
	private JDialog myGui;
	private JPanel visualizer;
	private JTextField nameTF;
	private JButton showButton;
	
	protected void setup() {
		myGui = new JDialog();
		JPanel panel = new JPanel();
		panel.add(new JLabel("Agent name: "));
		nameTF = new JTextField(40);
		panel.add(nameTF);
		showButton = new JButton("Show");
		showButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent ev) {
				try {
					String name = nameTF.getText();
					if (name != null) {
						AID id = new AID(name, AID.ISLOCALNAME);
						InternalValuesVisualizer ivv = InternalValuesVisualizer.createInternalValuesVisualizer(FieldViewerAgent.this, id);
						if (visualizer != null) {
							myGui.remove(visualizer);
						}
						myGui.add(ivv, BorderLayout.CENTER);
						visualizer = ivv;
						myGui.getContentPane().validate();
					}
				}
				catch (Exception e) {
					// FIXME: Show a warning dialog box
					e.printStackTrace();
				}
			}
		} );
		panel.add(showButton);
		myGui.getContentPane().add(panel, BorderLayout.NORTH);
		
		myGui.setSize(700, 200);
		myGui.setVisible(true);
	}
}
