/*****************************************************************
JADE - Java Agent DEvelopment Framework is a framework to develop 
multi-agent systems in compliance with the FIPA specifications.
Copyright (C) 2000 CSELT S.p.A. 

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

package jade.content.onto;

//#J2ME_EXCLUDE_FILE
//#APIDOC_EXCLUDE_FILE

class SlotKey {
	Class clazz;
	String slotName;
	private int hashcode;

	SlotKey(Class clazz, String slotName) {
		this.clazz = clazz;
		this.slotName = slotName;
		calcHashcode();
	}

	private void calcHashcode() {
		hashcode = 37;
		if (clazz != null) {
			hashcode ^= clazz.hashCode();
		}
		if (slotName != null) {
			hashcode ^= slotName.hashCode();
		}
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof SlotKey)) {
			return false;
		}
		SlotKey other = (SlotKey)obj;
		if (clazz != other.clazz) {
			return false;
		}
		if (slotName != null) {
			return slotName.equals(other.slotName);
		} else {
			return other.slotName == null;
		}
	}

	@Override
	public int hashCode() {
		return hashcode;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("SlotKey {clazz=");
		sb.append(clazz != null ? clazz.getSimpleName() : null);
		sb.append(" slotName=");
		sb.append(slotName);
		return sb.toString();
	}
}