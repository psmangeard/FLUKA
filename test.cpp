#include<iostream>
#include<fstream>
#include <sstream>
#include <vector>
#include <cstring>
#include <cmath>

#include "TCanvas.h"
#include "TROOT.h"
#include "TGraph.h"
#include "TAxis.h"

using namespace std;

int main() {
	FILE *fp;
    fp = fopen("./eventdata.txt","r");
	
	if (fp) {
	int oldevent;
	int numFile=0;
	int counter1, counter2;
	float x[10], y[10], z1[10], z2[10];																															//array to store coordinate points: z1 corresponds to z coordinate in non-bending plane, z2 is the z coordinate in the bending plane
	int numLines =0;
	char string[BUFSIZ];	
	for(size_t i = 0; fgets(string, sizeof(string),fp); ++i) {																												//loop over all lines of file
			numLines++;
			printf("%s", string);
			char  *line_elements[4];
			int n=0;
			char  *ptr;
			ptr = strtok (string," ,");
 			while (ptr!= NULL) {
			line_elements[n++] = ptr;
  			ptr = strtok(NULL, ",");
			}
			int event, id; 
			float f1, f2;
			event = atoi(line_elements[0]); id = atoi(line_elements[1]) ; f1 = atof(line_elements[2]); f2 = atof(line_elements[3]);								//converts first two elements of line into int type, next two into float type
			cout << "event= " << event << ", id= " << id << ", f1= " << f1 << ", f2=" << f2 << endl;
			if (i==0) {
				oldevent = event; }
			
				if (oldevent != event) {
				 			//graph the previous event, non-bending plane
				gROOT-> SetStyle("Plain");
				TCanvas *c1 = new TCanvas("c", Form("Event %d", oldevent),900,900);
				c1->Divide(2,1);																																//divide canvas into two pads
				TGraph *gr1 = new TGraph(counter1, x, z1);
				TGraph *gr2 = new TGraph(counter2, y, z2);
   				gr1->SetTitle(Form("Event %d: non-bending plane tracker hits", oldevent));
   				gr1->GetXaxis()->SetTitle("X (in cm)");
   				gr1->GetYaxis()->SetTitle("Z (in cm)");
				gr1->GetYaxis()->SetRangeUser(-17,17);
				gr1->GetXaxis()->SetLimits(-10,10);
   				gr1->SetMarkerStyle(kCircle);	
				c1->cd(1);
   				gr1->Draw("AP");

				//plot bending plane hits in the same canvas, different pad	

   				gr2->SetTitle(Form("Event %d: bending plane tracker hits", oldevent));
   				gr2->GetXaxis()->SetTitle("Y (in cm)");
   				gr2->GetYaxis()->SetTitle("Z (in cm)");
				gr2->GetYaxis()->SetRangeUser(-17,17);
				gr2->GetXaxis()->SetLimits(-10,10);
   				gr2->SetMarkerStyle(kCircle);	
				c1->cd(2);;
   				gr2->Draw("AP");
				c1->SaveAs(Form("./Event_plot/event_%d.jpg", numFile));
				c1->Update();
				oldevent = event;
				counter1 = 0;
			 	counter2 = 0;
				numFile++;
				cout << "file number #" << numFile << endl;
			}

			if (event == oldevent) {
				if ( id == 1 ) {																																//id 1 corresponds to non-bending plane hits
					counter1 = counter1+ 1;
					x[counter1-1] = f1;																															//stores coordinate into x array
					z1[counter1-1] = f2;																														//store coordinate into first z1 array
					cout << "The number of points in the non-bending plane is " << counter1 << endl;
					for (size_t k = 0; k < counter1; k++) {
					cout << "The x coordinates are " << x[k] << "  and the z coordinates are " << z1[k] << endl;
				}
			}	
				if (id == 2) {																																	//id 2 corresponds to bending-plane hits
					counter2 = counter2 + 1;	
					y[counter2-1] = f1;																														   //stores coordinate into y array
					z2[counter2-1] = f2;																													   //stores coordinate into z2 array
					cout << "The number of points in the bending plane is " << counter2 << endl;
					for (size_t k = 0; k < counter2; k++) {
						cout << "The y coordinates are " << y[k] << " and the z coordinates are " << z2[k] << endl;
					}
			}
		}

						
					}
				cout << "Total number of lines is " << numLines << endl;
								gROOT-> SetStyle("Plain");
				TCanvas *c1 = new TCanvas("c", Form("Event %d", oldevent),900,900);
				c1->Divide(2,1);																																//divide canvas into two pads
				TGraph *gr1 = new TGraph(counter1, x, z1);
				TGraph *gr2 = new TGraph(counter2, y, z2);
   				gr1->SetTitle(Form("Event %d: non-bending plane tracker hits", oldevent));
   				gr1->GetXaxis()->SetTitle("X (in cm)");
   				gr1->GetYaxis()->SetTitle("Z (in cm)");
				gr1->GetYaxis()->SetRangeUser(-17,17);
				gr1->GetXaxis()->SetLimits(-10,10);
   				gr1->SetMarkerStyle(kCircle);	
				c1->cd(1);
   				gr1->Draw("AP");

				//plot bending plane hits in the same canvas, different pad	

   				gr2->SetTitle(Form("Event %d: bending plane tracker hits", oldevent));
   				gr2->GetXaxis()->SetTitle("Y (in cm)");
   				gr2->GetYaxis()->SetTitle("Z (in cm)");
				gr2->GetYaxis()->SetRangeUser(-17,17);
				gr2->GetXaxis()->SetLimits(-10,10);
   				gr2->SetMarkerStyle(kCircle);	
				c1->cd(2);;
   				gr2->Draw("AP");
				c1->SaveAs(Form("./Event_plot/event_%d.jpg", numFile));
				c1->Update();
	}
	
	return 0;
}