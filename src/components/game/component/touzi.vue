<template>

  <div class="touzi " v-show="touzistatus.show">
    <div class="item animated rotateIn1">
      <img :src="point.p1" alt="">
    </div>
    <div class="item animated rotateIn1">
      <img :src="point.p2" alt="">
    </div>
  </div>

</template>

<script>
  import {mapGetters} from 'vuex'

  export default {
    data() {
      return {
      }
    },
    mounted() {

    },
    methods: {

    },
    computed: {
      ...mapGetters({
        'touzistatus': 'touzistatus'
      }),
      point(){
        let obj = {}
        if(this.touzistatus.point<=6){
          obj.p1 = require('../../../assets/touzi/1.png')
          obj.p2 =require('../../../assets/touzi/'+ (this.touzistatus.point-1)+'.png')
        }else{
          obj.p1 = require('../../../assets/touzi/6.png')
          obj.p2 =require('../../../assets/touzi/'+ (this.touzistatus.point-6)+'.png')
        }
        return obj


      }
    }
  }

</script>

<style scoped lang="scss">
  .touzi {
    position: fixed;
    top: 50%;
    left: 50%;
    -webkit-transform: translate(-50%, -50%);
    -moz-transform: translate(-50%, -50%);
    -ms-transform: translate(-50%, -50%);
    -o-transform: translate(-50%, -50%);
    transform: translate(-50%, -50%);
    z-index: 100000;
    .item {

      img {
        -webkit-transform: rotate(-90deg);
        -moz-transform: rotate(-90deg);
        -ms-transform: rotate(-90deg);
        -o-transform: rotate(-90deg);
        transform: rotate(-90deg);
        width: 1rem;

      }
    }
  }

  .rotateIn1 {
    -webkit-animation-duration: 1.6s;
    -moz-animation-duration: 1.6s;
    -o-animation-duration: 1.6s;
    animation-duration: 1.6s;
    -webkit-animation-name: rotateIn1;
    animation-name: rotateIn1;
  }

  @keyframes rotateIn1 {
    from {
      -webkit-transform-origin: center;
      transform-origin: center;
      -webkit-transform: rotate(-890deg) scale(.02);
      transform: rotate(-890deg) scale(.02);
    }

    to {
      -webkit-transform-origin: center;
      transform-origin: center;
      -webkit-transform: none;
      transform: none;
    }
  }

  @-webkit-keyframes rotateIn1 {
    from {
      -webkit-transform-origin: center;
      transform-origin: center;
      -webkit-transform: rotate(-890deg) scale(.02);
      transform: rotate(-890deg) scale(.02);
    }

    to {
      -webkit-transform-origin: center;
      transform-origin: center;
      -webkit-transform: none;
      transform: none;
    }
  }

</style>
