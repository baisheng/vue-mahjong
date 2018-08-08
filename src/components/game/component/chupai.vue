<template>

  <div :class="['chu_pai',site]" v-show="card.show">
    <div class="item">
      <img :src='card1' alt="">
    </div>
  </div>

</template>

<script>
  import {mapGetters} from 'vuex'

  export default {
    data() {
      return {}
    },
    mounted() {


    },
    methods: {},
    computed: {
      ...mapGetters({
        'card': 'showCard',
        'user_info': 'user_info',
        'members': 'members',

      }),
      card1() {

        return require('../../../assets/card4/' + this.card.card + '.png')
      },
      site() {
        let myId = this.user_info.id;
        let site = this.card.site



        if(this.members.length<4){
          return false
        }


        let user = this.members.find(v => v.PlayerId == myId)
        let other = this.members.find(v => v.position == site)


        let classObj = ''





        if (user.position - other.position == 1 || user.position - other.position == -3) {
          classObj = 'site4'
//          console.log('上家')
        } else if (other.position - user.position == 1 || other.position - user.position == -3) {
//          console.log('下家')
          classObj = 'site2'
        } else if (Math.abs(other.position - user.position) == 2) {
//          console.log('对家')
          classObj = 'site3'
        } else if (other.position == user.position) {
//          console.log('自家')
          classObj = 'site1'
        }


        return classObj


      }
    }

  }

</script>

<style scoped lang="scss">

  .chu_pai {
    position: fixed;
    border: 1px solid #01eabd;
    border-radius: .06rem;
    box-shadow: 0 0 15px #01eabd;
    padding: .01rem .08rem;
    z-index: 100;
    background: rgba(0, 0, 0, .3);
    img {
      width: .86rem;
    }

    &.site1 {
      left: 60%;
      top: 48%;
      -webkit-transform: rotate(-90deg);
      -moz-transform: rotate(-90deg);
      -ms-transform: rotate(-90deg);
      -o-transform: rotate(-90deg);
      transform: rotate(-90deg);

    }
    &.site2 {

      left: 50%;
      margin-left: -.4rem;
      top: 15%;

    }
    &.site4 {

      left: 50%;
      margin-left: -.4rem;
      top: 75%;

    }
    &.site3 {
      left: 30%;
      top: 46%;
      -webkit-transform: rotate(90deg);
      -moz-transform: rotate(90deg);
      -ms-transform: rotate(90deg);
      -o-transform: rotate(90deg);
      transform: rotate(90deg);

    }
  }

</style>
