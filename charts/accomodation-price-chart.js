function accomodationPriceChart(accomodations) {
  const accomodationPrice = {
    lessThan500: 0,
    lessThan1000: 0,
    lessThan2000: 0,
    lessThan4000: 0,
    moreThan3999: 0
  };

  accomodations.forEach(accomodation => {
    if (accomodation.price < 500) {
      accomodationPrice.lessThan500 += 1;
    } else if (accomodation.price < 1000) {
      accomodationPrice.lessThan1000 += 1;
    } else if (accomodation.price < 2000) {
      accomodationPrice.lessThan2000 += 1;
    } else if (accomodation.price < 4000) {
      accomodationPrice.lessThan4000 += 1;
    } else if (accomodation.price > 3999) {
      accomodationPrice.moreThan3999 += 1;
    }
  });

  Highcharts.chart('accomodation-price-chart', {
    chart: {
      type: 'pie'
    },
    title: {
        text: 'Accomodation price'
    },
    series: [{
      name: 'Brands',
      innerSize: '40%',
      colorByPoint: true,
      data: [{
          name: 'Less than 500',
          y: accomodationPrice.lessThan500,
      }, {
          name: 'Less than 1000',
          y: accomodationPrice.lessThan1000
      }, {
          name: 'Less than 2000',
          y: accomodationPrice.lessThan2000
      }, {
          name: 'Less than 4000',
          y: accomodationPrice.lessThan4000
      }, {
          name: 'More than 3999',
          y: accomodationPrice.moreThan3999
      }]
    }]
  });
}