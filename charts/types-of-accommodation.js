function makeChartForAccommodationTypes(accommodations) {
  const typeOfAccommodation = {
    privateRoom: 0,
    entireHome: 0,
    sharedRoom: 0
  };

  accommodations.forEach(accommodation => {
    if (accommodation.room_type === 'Private room') {
      typeOfAccommodation.privateRoom += 1;
    } else if (accommodation.room_type === 'Entire home/apt') {
      typeOfAccommodation.entireHome += 1;
    } else if (accommodation.room_type === 'Shared room') {
      typeOfAccommodation.sharedRoom += 1;
    }
  });

  Highcharts.chart('types-of-accommodation-chart', {
    chart: {
      type: 'column'
    },
    title: {
        text: 'Accommodation types'
    },
    xAxis: {
      type: 'category'
    },
    yAxis: {
      title: {
        text: null
      }
    },
    legend: {
      enabled: false
    },
    series: [
      {
        name: 'Accommodation types',
        colorByPoint: true,
        data: [{
          name: 'Private room',
          y: typeOfAccommodation.privateRoom
        }, {
          name: 'Entire home/apt',
          y: typeOfAccommodation.entireHome
        }, {
          name: 'Shared room',
          y: typeOfAccommodation.sharedRoom
        }]
      }]
  });
}