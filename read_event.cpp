#include<iostream>
#include<fstream>
#include <sstream>
#include <vector>
#include <cstring>
#include <cmath>
using namespace std;
//This is a C++ program reproducing the read_track.for code. It reads the .99 output file from a FLUKA run, checks for trigger conditions, 
//and rewrites the bending and non-bending plane coordinate for all good events onto a txt file "eventdata.txt" 
//To run on the terminal type command: g++ -std=c++0x -o read_event read_event.cpp


int main () {
	ifstream inFile("./fort.99");																			//.99 file outputted by the FLUKA run
	ofstream outFile1;	
	outFile1.open ("./eventdata.txt", outFile1.out);											//file in which we write the good events coordinate hits					
	float x[500]; float y[500]; float z[500];    															//create an array to point along the tracks, the coordinates of boundary crossings points will be stored in it
	const size_t size = 200000;
	string line[size];		
	size_t i = 0;
	int icn = 0;																							//icn counts the number of boundary crossings per event of the detectors in the system
	int iold = 1;																							//variable that stores old event number
	if(!inFile) {
    	cout<<"Error opening output file"<<endl;
    	system("pause");
    	return -1;
		}
	
	while(!inFile.eof() && i < size) {
		getline(inFile, line[i]);																		//read next line into the next string
		i++;
	}
	size_t numLines = i;
	for (int i=0; i< numLines; ++i) {																	//loop over all lines in the file
			vector<char> char_line(line[i].begin(), line[i].end());										// converts the string into a writable char* type
			char_line.push_back('\0');
			//cout << &char_line[0] << endl;															//prints each line
		    char *line_elements[9];																		//stores all elements of a line into an array
			char * p;													
			size_t n = 0;																						
			for (p = strtok(&char_line[0], " "); p; p = strtok(NULL, " "))	{											//reads each element of a line and parses it
    	    if (n >= 9) {																			    				//maximum number of storable tokens exceeded
        		break;
			}
			line_elements[n++] = p; 																			//create an array holding the elements of a line
			}
			int i1,i2,i3,i4;								
		    float f1, f2, f3, f4, f5;
			i1 = atoi (line_elements[0]); i2 = atoi(line_elements[1]); i3 = atoi(line_elements[2]); i4 = atoi (line_elements[3]);				//converts the first 4 char-type elements of the line as integers 
			f1 = stof(line_elements[4]); f2 = stof(line_elements[5]); f3 = stof(line_elements[6]); f4 = stof(line_elements[7]); f5 = stof(line_elements[8]);	//converts the rest of the elements as floats 
			if(f4 > 33.499) {
			float energy = f1;																													//stores the energy of the particle
			//cout << "Printing the energy of the particle: " << energy << endl;
			}
			if (i1 == iold) {																													//still reading the same event
				//cout << "Still reading the same event" << endl;
													//these are the trigger requirements on T1, T3, T4 and the guard
						if(i4 !=7)		{																//photons are not plotted
							int mreg = i2; int newreg = i3 ;											//we assign variable mreg the old region ID, variable newreg new region ID
							//check to see if the particle has crossed one of the boundaries (r19=air, r1=T1, r6=T3, r7=guard, r11=Tracker1 ,..., r17=Tracker7, r18=T4)
							if ((mreg == 19 && newreg == 1) || (mreg == 19 && newreg == 6) || (mreg == 19 && newreg == 7) ||  (mreg == 19 && newreg == 11) || (mreg == 19 && newreg == 12) || (mreg == 19 && newreg == 13) ||	
								(mreg == 19 && newreg == 14) || (mreg == 19 && newreg == 15) || (mreg == 19 && newreg == 16) || (mreg == 19 && newreg == 17) || (mreg == 19 && newreg == 18)) {
									//cout << "The particle has crossed a boundary from region " << mreg << " to region " << newreg << endl;
									icn = icn + 1;	 											// icn is the number of boundary crossings
									//cout << "Boundary crossing #" << icn << endl;
									x[(icn-1)] = f2;											//stores the xyz coordinates at the boundary crossing
									y[(icn-1)] = f3;											//stores the xyz coordinates at the boundary crossing
									z[(icn-1)] = f4;											//stores the xyz coordinates at the boundary crossing

							}
					}
			    }
		  
			else {																																		//if the next line is a new event, check trigger requirements on old event
				int it1 = 0; int it3 = 0; int it4 = 0; int itg = 0;	
					for (size_t k=0; k < icn; k++) { 																									//we loop over all boundary crossing points for the previous event
										if((z[k] > 33.1 && z[k] < 33.6) && (sqrt(pow(x[k],2.0) + pow(y[k],2.0)) < 13.5)) {								//did the particle cross T1?
											it1 = 1;
										}
										if((z[k] > 0.1 && z[k] < 0.6) && (sqrt(pow(x[k],2.0) + pow(y[k],2.0)) < 3.5)) {									//did the particle cross T3?
											it3 = 1;
										}
										if((z[k] > -23.5 && z[k] < -22.9) && (sqrt(pow(x[k],2.0) + pow(y[k],2.0)) < 18.0)) {								//did the particle cross T4?
											it4 = 1; 
									}
										if ((z[k] > 0.1 && z[k] < 0.6) && (sqrt( pow(x[k],2.0) + pow(y[k],2.0)) < 3.5)) {									//did the particle cross the guard?
											itg = 1;
										}
					               }
										cout << "it1=" << it1 << ", it3=" << it3 << ", it4=" << it4 << ", itg=" << itg << endl;
										if(it1 == 1 && it3 == 1 && it4 == 1 && icn > 1) {		//did a trigger condition occur? If yes,  write the coordinates onto new text file and define bending and non-bending plane crossings 		
											cout << "TRIGGER!!! " << endl;
					


											for (size_t k=0; k < icn; k++) {
											if((z[k] > -2.0 && z[k] < -1.0) || (z[k] > -18.0 && z[k] < -17.0) || (z[k] > -22.0 && z[k] < -21.0)) {				//non bending plane tracker hits
												cout <<"Event #" << iold << ", crossing #" << k << "; non-bending X coordinate is: " << x[k] << "; non-bending Z coordinate is:   " << z[k] + 9.5  << endl; 	//move the mag bore center to (0,0,0) for plotting purposes 
												outFile1 << iold << ",1," << x[k] << "," << z[k] + 9.5 << endl;						    // identifier 1 corresponds to non-bending plane; write event in output text file
											}
										}
											for (size_t k=0; k < icn; k++) { 
											if((z[k] > -4.0 && z[k] < -3.0) || (z[k] > -10.0 && z[k] < -9.0) || (z[k] > -16.0 && z[k] < -15.0) || (z[k] > -20.0 && z[k] < -19.0)) {				//bending plane tracker hits
													cout <<"Event #" << iold << ", crossing #" << k <<"; bending Y coordinate is: " << y[k] << "; bending Z coordinate is:  " << z[k] + 9.5 << endl; 											//move mag bore center to (0,0,0)
												outFile1 <<iold << ",2," << y[k] << "," << z[k] + 9.5 << endl;						    // identifier 2 corresponds to bending plane; write event in output text file
												} 
											}
										}
										else {
											cout << "No trigger conditions met for this event" << endl;
											}
				
			iold = i1;																													//update the event number
			cout << "Reading new event " << iold << endl;
			icn = 0;																													//reset boundary crossing counter to 0
			i--;																														//go back one line, to first line of new event
			}
	}
     	outFile1.close();
	    return 0;
}



