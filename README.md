# shiny_sample
This sample shiny dashboard uses [Licenced and Certified Healthcare Facilities](https://data.chhs.ca.gov/dataset/healthcare-facility-locations/resource/098bbc36-044d-441f-9442-1f4db4d8aaa0) data maintained by the California Department of Public Health.

The first page of the app, "**County Measures**", displays information about the user's selected county. It utilizes `leaflet`, `reactable`, `plotly` and `shinydashboard` widgets:    

![p1a](https://github.com/sgrever/shiny_sample/assets/65444406/343ef57c-352f-406b-b079-5358ac38a6e8)
![p1b](https://github.com/sgrever/shiny_sample/assets/65444406/0cb51ca0-7556-4b7e-a3d3-5659ec9d7f51)


The second tab of the app, "**State Measures**" produces a summary of the entire statewide dataset. The top plot displays license expiration dates categorized into yearly quarters. The table shows the full dataset, which can be filtered and downloaded by the user. This page utilizes `plotly` and `reactable` widgets:    

![p2a](https://github.com/sgrever/shiny_sample/assets/65444406/efce6a31-e610-476f-a3f9-34fb8f2d0d9b)
![p2b](https://github.com/sgrever/shiny_sample/assets/65444406/9686f746-4c03-4c6b-ac46-05bf8681b780)
![p2c](https://github.com/sgrever/shiny_sample/assets/65444406/9a3172dc-a5a6-497f-9872-98d8a2be71b3)
