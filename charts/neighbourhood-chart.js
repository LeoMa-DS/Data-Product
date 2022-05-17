function neighbourhoodChart(accomodations) {
  // Extract labels
  const neighbourhoodLabels = Array.from(new Set(accomodations.map(accomodation => accomodation.neighbourhood))).sort();

  // Store the data for the
  const data = {
    privateRooms: {},
    entireHome: {},
    shared: {}
  };

  // Populate the data
  accomodations.forEach(accomodation => {
    if (accomodation.room_type === 'Private room') {
      if (data.privateRooms[accomodation.neighbourhood] === undefined) {
        data.privateRooms[accomodation.neighbourhood] = 1;
      } else {
        data.privateRooms[accomodation.neighbourhood] += 1;
      }
    } else if (accomodation.room_type === 'Entire home/apt') {
      if (data.entireHome[accomodation.neighbourhood] === undefined) {
        data.entireHome[accomodation.neighbourhood] = 1;
      } else {
        data.entireHome[accomodation.neighbourhood] += 1;
      }
    } else if (accomodation.room_type === 'Shared room') {
      if (data.shared[accomodation.neighbourhood] === undefined) {
        data.shared[accomodation.neighbourhood] = 1;
      } else {
        data.shared[accomodation.neighbourhood] += 1;
      }
    }
  });

  // Extract the data
  const privateRooms = Object.keys(data.privateRooms).sort().map(neighbourhood => data.privateRooms[neighbourhood]);
  const entireHome = Object.keys(data.entireHome).sort().map(neighbourhood => data.entireHome[neighbourhood]);
  const shared = Object.keys(data.shared).sort().map(neighbourhood => data.shared[neighbourhood]);

  Highcharts.chart('neighbourhood-chart', {
    chart: {
      type: 'area'
    },
    xAxis: {
      categories: neighbourhoodLabels
    },
    title: {
      text: 'Neighbourhood'
    },
    series: [{
      name: 'Private room',
      data: privateRooms
    }, {
      name: 'Entire home/apt',
      data: entireHome
    }, {
      name: 'Shared room',
      data: shared
    }]
  });
}